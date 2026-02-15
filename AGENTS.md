# OpenWeatherMap API Client

## Description

This is a Common Lisp client for multiple OpenWeatherMap APIs. It is generated from API documentation. This is a library meant to be used in other Lisp applications.

## Tech stack

- Language: Common Lisp

## Project rules

- Update this file as the project develops.
- README.org will be in org-mode.
- All plans will be kept in the `/plans` directory as markdown files.
- Keep a comprehensive `CHANGELOG.md` with all changes for a session.
- **This project must be well tested**. There should be a comprehensive test suite.
    - In addition to unit tests, we will develop a suite of intergration tests against the live API, which are run separately.
- This project must have a good development story, with a comprehensive Makefile in addition to idiomatic Common Lisp patterns for development (running tests, etc.)
- This project should follow idiomatic Common Lisp best practices.
- Extensively read the API documentation and create OpenAPI / Swagger files for each supported OpenWeatherMap API family.

## API Updates

Please research and create OpenAPI docs for the following:

- https://openweathermap.org/api/one-call-3?collection=one_call_api_3.0
- https://openweathermap.org/current?collection=current_forecast
- https://openweathermap.org/forecast5?collection=current_forecast
- https://openweathermap.org/api/air-pollution?collection=environmental
- https://openweathermap.org/api/weathermaps?collection=maps
- https://openweathermap.org/api/geocoding-api?collection=other

