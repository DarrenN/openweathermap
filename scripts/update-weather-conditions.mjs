#!/usr/bin/env node
// SPDX-License-Identifier: MIT

import fs from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';

const ROOT = process.cwd();
const DATA_PATH = path.join(ROOT, 'data', 'weather-conditions.json');
const SCHEMA_PATH = path.join(ROOT, 'data', 'weather-conditions.schema.json');
const LISP_PATH = path.join(ROOT, 'src', 'data', 'weather-conditions.lisp');

const SOURCE_URLS = [
  'https://openweathermap.org/weather-conditions',
  'https://docs.openweather.co.uk/weather-conditions'
];

const ICONS = [
  { key: '01', day: '01d', night: '01n', description: 'clear sky' },
  { key: '02', day: '02d', night: '02n', description: 'few clouds' },
  { key: '03', day: '03d', night: '03n', description: 'scattered clouds' },
  { key: '04', day: '04d', night: '04n', description: 'broken clouds' },
  { key: '09', day: '09d', night: '09n', description: 'shower rain' },
  { key: '10', day: '10d', night: '10n', description: 'rain' },
  { key: '11', day: '11d', night: '11n', description: 'thunderstorm' },
  { key: '13', day: '13d', night: '13n', description: 'snow' },
  { key: '50', day: '50d', night: '50n', description: 'mist' }
];

const CONDITIONS = [
  { id: 200, group: 'Thunderstorm', description: 'thunderstorm with light rain', icon: '11d' },
  { id: 201, group: 'Thunderstorm', description: 'thunderstorm with rain', icon: '11d' },
  { id: 202, group: 'Thunderstorm', description: 'thunderstorm with heavy rain', icon: '11d' },
  { id: 210, group: 'Thunderstorm', description: 'light thunderstorm', icon: '11d' },
  { id: 211, group: 'Thunderstorm', description: 'thunderstorm', icon: '11d' },
  { id: 212, group: 'Thunderstorm', description: 'heavy thunderstorm', icon: '11d' },
  { id: 221, group: 'Thunderstorm', description: 'ragged thunderstorm', icon: '11d' },
  { id: 230, group: 'Thunderstorm', description: 'thunderstorm with light drizzle', icon: '11d' },
  { id: 231, group: 'Thunderstorm', description: 'thunderstorm with drizzle', icon: '11d' },
  { id: 232, group: 'Thunderstorm', description: 'thunderstorm with heavy drizzle', icon: '11d' },

  { id: 300, group: 'Drizzle', description: 'light intensity drizzle', icon: '09d' },
  { id: 301, group: 'Drizzle', description: 'drizzle', icon: '09d' },
  { id: 302, group: 'Drizzle', description: 'heavy intensity drizzle', icon: '09d' },
  { id: 310, group: 'Drizzle', description: 'light intensity drizzle rain', icon: '09d' },
  { id: 311, group: 'Drizzle', description: 'drizzle rain', icon: '09d' },
  { id: 312, group: 'Drizzle', description: 'heavy intensity drizzle rain', icon: '09d' },
  { id: 313, group: 'Drizzle', description: 'shower rain and drizzle', icon: '09d' },
  { id: 314, group: 'Drizzle', description: 'heavy shower rain and drizzle', icon: '09d' },
  { id: 321, group: 'Drizzle', description: 'shower drizzle', icon: '09d' },

  { id: 500, group: 'Rain', description: 'light rain', icon: '10d' },
  { id: 501, group: 'Rain', description: 'moderate rain', icon: '10d' },
  { id: 502, group: 'Rain', description: 'heavy intensity rain', icon: '10d' },
  { id: 503, group: 'Rain', description: 'very heavy rain', icon: '10d' },
  { id: 504, group: 'Rain', description: 'extreme rain', icon: '10d' },
  { id: 511, group: 'Rain', description: 'freezing rain', icon: '13d' },
  { id: 520, group: 'Rain', description: 'light intensity shower rain', icon: '09d' },
  { id: 521, group: 'Rain', description: 'shower rain', icon: '09d' },
  { id: 522, group: 'Rain', description: 'heavy intensity shower rain', icon: '09d' },
  { id: 531, group: 'Rain', description: 'ragged shower rain', icon: '09d' },

  { id: 600, group: 'Snow', description: 'light snow', icon: '13d' },
  { id: 601, group: 'Snow', description: 'snow', icon: '13d' },
  { id: 602, group: 'Snow', description: 'heavy snow', icon: '13d' },
  { id: 611, group: 'Snow', description: 'sleet', icon: '13d' },
  { id: 612, group: 'Snow', description: 'light shower sleet', icon: '13d' },
  { id: 613, group: 'Snow', description: 'shower sleet', icon: '13d' },
  { id: 615, group: 'Snow', description: 'light rain and snow', icon: '13d' },
  { id: 616, group: 'Snow', description: 'rain and snow', icon: '13d' },
  { id: 620, group: 'Snow', description: 'light shower snow', icon: '13d' },
  { id: 621, group: 'Snow', description: 'shower snow', icon: '13d' },
  { id: 622, group: 'Snow', description: 'heavy shower snow', icon: '13d' },

  { id: 701, group: 'Atmosphere', description: 'mist', icon: '50d' },
  { id: 711, group: 'Atmosphere', description: 'smoke', icon: '50d' },
  { id: 721, group: 'Atmosphere', description: 'haze', icon: '50d' },
  { id: 731, group: 'Atmosphere', description: 'sand/dust whirls', icon: '50d' },
  { id: 741, group: 'Atmosphere', description: 'fog', icon: '50d' },
  { id: 751, group: 'Atmosphere', description: 'sand', icon: '50d' },
  { id: 761, group: 'Atmosphere', description: 'dust', icon: '50d' },
  { id: 762, group: 'Atmosphere', description: 'volcanic ash', icon: '50d' },
  { id: 771, group: 'Atmosphere', description: 'squalls', icon: '50d' },
  { id: 781, group: 'Atmosphere', description: 'tornado', icon: '50d' },

  { id: 800, group: 'Clear', description: 'clear sky', icon: '01d' },

  { id: 801, group: 'Clouds', description: 'few clouds: 11-25%', icon: '02d' },
  { id: 802, group: 'Clouds', description: 'scattered clouds: 25-50%', icon: '03d' },
  { id: 803, group: 'Clouds', description: 'broken clouds: 51-84%', icon: '04d' },
  { id: 804, group: 'Clouds', description: 'overcast clouds: 85-100%', icon: '04d' }
];

const SCHEMA = {
  $schema: 'https://json-schema.org/draft/2020-12/schema',
  $id: 'https://github.com/yuzu/openweathermap/schemas/weather-conditions.schema.json',
  title: 'OpenWeather condition/icon mapping dataset',
  type: 'object',
  required: ['schema_version', 'generated_at', 'source_urls', 'icon_base_url', 'icon_sets', 'conditions'],
  properties: {
    schema_version: { type: 'integer', minimum: 1 },
    generated_at: { type: 'string', format: 'date-time' },
    source_urls: { type: 'array', minItems: 1, items: { type: 'string', format: 'uri' } },
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

function escapeLispString(value) {
  return value
    .replaceAll('\\', '\\\\')
    .replaceAll('"', '\\"');
}

function assertShape(dataset) {
  if (!Array.isArray(dataset.icon_sets) || dataset.icon_sets.length === 0) {
    throw new Error('Dataset must include non-empty icon_sets array.');
  }
  if (!Array.isArray(dataset.conditions) || dataset.conditions.length === 0) {
    throw new Error('Dataset must include non-empty conditions array.');
  }

  const iconSet = new Set();
  for (const icon of dataset.icon_sets) {
    for (const k of ['key', 'day', 'night', 'description']) {
      if (typeof icon[k] !== 'string' || icon[k].length === 0) {
        throw new Error(`Invalid icon entry: missing ${k}`);
      }
    }
    iconSet.add(icon.day);
    iconSet.add(icon.night);
  }

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
  }
}

async function fetchUpstreamHtml(url) {
  const response = await fetch(url, {
    headers: {
      'user-agent': 'openweathermap-phase-g1-updater'
    }
  });
  if (!response.ok) {
    throw new Error(`Failed to fetch ${url}: ${response.status}`);
  }
  return response.text();
}

function assertUpstreamMentions(html, dataset) {
  const requiredFragments = [
    'Weather condition codes',
    'Icon list',
    '01d',
    '11d',
    '500',
    '800',
    '804'
  ];
  for (const fragment of requiredFragments) {
    if (!html.includes(fragment)) {
      throw new Error(`Upstream page does not contain expected marker: ${fragment}`);
    }
  }

  const sampleIds = dataset.conditions
    .filter((row) => [200, 500, 600, 800, 804].includes(row.id))
    .map((row) => String(row.id));

  for (const id of sampleIds) {
    if (!html.includes(id)) {
      throw new Error(`Upstream page missing expected weather condition id marker: ${id}`);
    }
  }
}

function buildDataset() {
  return {
    schema_version: 1,
    generated_at: new Date().toISOString(),
    source_urls: SOURCE_URLS,
    icon_base_url: 'https://openweathermap.org/payload/api/media/file',
    icon_sets: ICONS,
    conditions: [...CONDITIONS].sort((a, b) => a.id - b.id)
  };
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

async function main() {
  const dataset = buildDataset();
  assertShape(dataset);

  let fetched = 0;
  let lastError = null;
  for (const url of SOURCE_URLS) {
    try {
      const html = await fetchUpstreamHtml(url);
      assertUpstreamMentions(html, dataset);
      fetched += 1;
      break;
    } catch (err) {
      lastError = err;
    }
  }

  if (fetched === 0) {
    throw new Error(`Unable to validate against upstream weather-conditions docs: ${lastError?.message ?? 'unknown error'}`);
  }

  await fs.mkdir(path.dirname(DATA_PATH), { recursive: true });
  await fs.mkdir(path.dirname(LISP_PATH), { recursive: true });

  const schemaText = JSON.stringify(SCHEMA, null, 2) + '\n';
  await fs.writeFile(SCHEMA_PATH, schemaText, 'utf8');

  const jsonText = JSON.stringify(dataset, null, 2) + '\n';
  await fs.writeFile(DATA_PATH, jsonText, 'utf8');

  const lispText = renderLisp(dataset);
  await fs.writeFile(LISP_PATH, lispText, 'utf8');

  console.log(`Wrote ${SCHEMA_PATH}`);
  console.log(`Wrote ${DATA_PATH}`);
  console.log(`Wrote ${LISP_PATH}`);
}

main().catch((err) => {
  console.error(err instanceof Error ? err.message : String(err));
  process.exit(1);
});
