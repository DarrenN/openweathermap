#!/usr/bin/env node
// SPDX-License-Identifier: MIT

import fs from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';
import { load as loadHtml } from 'cheerio';

const ROOT = process.cwd();
const DATA_PATH = path.join(ROOT, 'data', 'weather-conditions.json');
const SCHEMA_PATH = path.join(ROOT, 'data', 'weather-conditions.schema.json');
const LISP_PATH = path.join(ROOT, 'src', 'data', 'weather-conditions.lisp');

const SOURCE_URLS = [
  'https://openweathermap.org/weather-conditions',
  'https://docs.openweather.co.uk/weather-conditions'
];

const SCHEMA = {
  $schema: 'https://json-schema.org/draft/2020-12/schema',
  $id: 'https://github.com/yuzu/openweathermap/schemas/weather-conditions.schema.json',
  title: 'OpenWeather condition/icon mapping dataset',
  type: 'object',
  required: ['schema_version', 'generated_at', 'source_urls', 'source_used', 'icon_base_url', 'icon_sets', 'conditions'],
  properties: {
    schema_version: { type: 'integer', minimum: 1 },
    generated_at: { type: 'string', format: 'date-time' },
    source_urls: { type: 'array', minItems: 1, items: { type: 'string', format: 'uri' } },
    source_used: { type: 'string', format: 'uri' },
    icon_base_url: { type: 'string', format: 'uri' },
    icon_sets: {
      type: 'array',
      minItems: 1,
      items: {
        type: 'object',
        required: ['key', 'day', 'night', 'description'],
        properties: {
          key: { type: 'string' },
          day: { type: 'string' },
          night: { type: 'string' },
          description: { type: 'string' }
        },
        additionalProperties: false
      }
    },
    conditions: {
      type: 'array',
      minItems: 1,
      items: {
        type: 'object',
        required: ['id', 'group', 'description', 'icon'],
        properties: {
          id: { type: 'integer' },
          group: { type: 'string' },
          description: { type: 'string' },
          icon: { type: 'string' }
        },
        additionalProperties: false
      }
    }
  },
  additionalProperties: false
};

function normalizeWhitespace(value) {
  return value.replace(/\s+/g, ' ').trim();
}

function escapeLispString(value) {
  return value
    .replaceAll('\\', '\\\\')
    .replaceAll('"', '\\"');
}

async function fetchUpstreamHtml(url) {
  const response = await fetch(url, {
    headers: {
      'user-agent': 'openweathermap-phase-g-updater'
    }
  });
  if (!response.ok) {
    throw new Error(`Failed to fetch ${url}: ${response.status}`);
  }
  return response.text();
}

function parseIconSetsFromHtml(html) {
  const $ = loadHtml(html);
  const rows = [];

  $('section#Icon-list table tr').each((_idx, tr) => {
    const cells = $(tr).find('td');
    if (cells.length < 3) {
      return;
    }

    const dayText = normalizeWhitespace($(cells[0]).text());
    const nightText = normalizeWhitespace($(cells[1]).text());
    const descriptionText = normalizeWhitespace($(cells[2]).text());

    const dayMatch = dayText.match(/\b(\d{2}[dn])(?:\.png)?\b/i);
    const nightMatch = nightText.match(/\b(\d{2}[dn])(?:\.png)?\b/i);

    if (!dayMatch || !nightMatch || descriptionText.length === 0) {
      return;
    }

    const day = dayMatch[1].toLowerCase();
    const night = nightMatch[1].toLowerCase();

    rows.push({
      key: day.slice(0, 2),
      day,
      night,
      description: descriptionText
    });
  });

  const deduped = new Map();
  for (const row of rows) {
    deduped.set(row.key, row);
  }

  return [...deduped.values()].sort((a, b) => a.key.localeCompare(b.key));
}

function parseConditionsFromHtml(html) {
  const $ = loadHtml(html);
  const rows = [];

  $('table tr').each((_idx, tr) => {
    const cells = $(tr).find('td');
    if (cells.length < 4) {
      return;
    }

    const idText = normalizeWhitespace($(cells[0]).text());
    if (!/^\d{3}$/.test(idText)) {
      return;
    }

    const group = normalizeWhitespace($(cells[1]).text());
    const description = normalizeWhitespace($(cells[2]).text());
    const iconCellText = normalizeWhitespace($(cells[3]).text());

    const icons = [...iconCellText.matchAll(/(\d{2}[dn])(?:\.png)?/ig)]
      .map((m) => m[1].toLowerCase());

    if (group.length === 0 || description.length === 0 || icons.length === 0) {
      return;
    }

    const preferredIcon = icons.find((code) => code.endsWith('d')) || icons[0];

    rows.push({
      id: Number.parseInt(idText, 10),
      group,
      description,
      icon: preferredIcon
    });
  });

  const deduped = new Map();
  for (const row of rows) {
    deduped.set(row.id, row);
  }

  return [...deduped.values()].sort((a, b) => a.id - b.id);
}

function assertShape(dataset) {
  if (!Array.isArray(dataset.icon_sets) || dataset.icon_sets.length === 0) {
    throw new Error('Dataset must include non-empty icon_sets array.');
  }
  if (!Array.isArray(dataset.conditions) || dataset.conditions.length === 0) {
    throw new Error('Dataset must include non-empty conditions array.');
  }

  if (dataset.icon_sets.length < 9) {
    throw new Error(`Expected at least 9 icon sets, got ${dataset.icon_sets.length}.`);
  }

  if (dataset.conditions.length < 50) {
    throw new Error(`Expected at least 50 weather conditions, got ${dataset.conditions.length}.`);
  }

  const iconSet = new Set();
  for (const icon of dataset.icon_sets) {
    for (const k of ['key', 'day', 'night', 'description']) {
      if (typeof icon[k] !== 'string' || icon[k].length === 0) {
        throw new Error(`Invalid icon entry: missing ${k}`);
      }
    }
    if (icon.day.slice(0, 2) !== icon.key || icon.night.slice(0, 2) !== icon.key) {
      throw new Error(`Icon row key/day/night mismatch for key ${icon.key}.`);
    }
    iconSet.add(icon.day);
    iconSet.add(icon.night);
  }

  const ids = new Set();
  for (const row of dataset.conditions) {
    if (!Number.isInteger(row.id)) {
      throw new Error(`Condition id must be integer: ${JSON.stringify(row)}`);
    }
    if (typeof row.group !== 'string' || row.group.length === 0) {
      throw new Error(`Condition group missing for id ${row.id}`);
    }
    if (typeof row.description !== 'string' || row.description.length === 0) {
      throw new Error(`Condition description missing for id ${row.id}`);
    }
    if (typeof row.icon !== 'string' || row.icon.length === 0) {
      throw new Error(`Condition icon missing for id ${row.id}`);
    }
    if (!iconSet.has(row.icon)) {
      throw new Error(`Condition icon ${row.icon} is not declared in icon_sets.`);
    }
    if (ids.has(row.id)) {
      throw new Error(`Duplicate condition id found: ${row.id}`);
    }
    ids.add(row.id);
  }

  for (const requiredId of [200, 500, 600, 800, 804]) {
    if (!ids.has(requiredId)) {
      throw new Error(`Expected sentinel weather condition ID ${requiredId} is missing.`);
    }
  }

  for (const requiredIcon of ['01d', '10d', '11d', '13d', '50d']) {
    if (!iconSet.has(requiredIcon)) {
      throw new Error(`Expected sentinel icon ${requiredIcon} is missing.`);
    }
  }
}

function renderLisp(dataset) {
  const conditionRows = dataset.conditions
    .map((row) => `   (:id ${row.id} :group "${escapeLispString(row.group)}" :description "${escapeLispString(row.description)}" :icon "${escapeLispString(row.icon)}")`)
    .join('\n');

  const iconRows = dataset.icon_sets
    .map((row) => `   (:key "${escapeLispString(row.key)}" :day "${escapeLispString(row.day)}" :night "${escapeLispString(row.night)}" :description "${escapeLispString(row.description)}")`)
    .join('\n');

  return `;; SPDX-License-Identifier: MIT
;; Auto-generated by scripts/update-weather-conditions.mjs. Do not edit by hand.

(in-package :openweathermap)

(defparameter *weather-condition-icons*
  '(
${iconRows}
   ))

(defparameter *weather-condition-catalog*
  '(
${conditionRows}
   ))
`;
}

function buildDataset(parsed, sourceUrl) {
  return {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    source_urls: SOURCE_URLS,
    source_used: sourceUrl,
    icon_base_url: 'https://openweathermap.org/payload/api/media/file',
    icon_sets: parsed.icon_sets,
    conditions: parsed.conditions
  };
}

async function parseFromSource(url) {
  const html = await fetchUpstreamHtml(url);

  const icon_sets = parseIconSetsFromHtml(html);
  const conditions = parseConditionsFromHtml(html);

  return { icon_sets, conditions };
}

async function main() {
  let dataset = null;
  const errors = [];

  for (const url of SOURCE_URLS) {
    try {
      const parsed = await parseFromSource(url);
      const candidate = buildDataset(parsed, url);
      assertShape(candidate);
      dataset = candidate;
      break;
    } catch (err) {
      errors.push(`${url}: ${err instanceof Error ? err.message : String(err)}`);
    }
  }

  if (!dataset) {
    throw new Error(`Unable to parse weather conditions from upstream docs. Errors: ${errors.join(' | ')}`);
  }

  await fs.mkdir(path.dirname(DATA_PATH), { recursive: true });
  await fs.mkdir(path.dirname(LISP_PATH), { recursive: true });

  const schemaText = JSON.stringify(SCHEMA, null, 2) + '\n';
  await fs.writeFile(SCHEMA_PATH, schemaText, 'utf8');

  const jsonText = JSON.stringify(dataset, null, 2) + '\n';
  await fs.writeFile(DATA_PATH, jsonText, 'utf8');

  const lispText = renderLisp(dataset);
  await fs.writeFile(LISP_PATH, lispText, 'utf8');

  console.log(`Parsed weather conditions from ${dataset.source_used}`);
  console.log(`Wrote ${SCHEMA_PATH}`);
  console.log(`Wrote ${DATA_PATH}`);
  console.log(`Wrote ${LISP_PATH}`);
  console.log(`Counts: ${dataset.conditions.length} conditions, ${dataset.icon_sets.length} icon sets`);
}

main().catch((err) => {
  console.error(err instanceof Error ? err.message : String(err));
  process.exit(1);
});
