# CLAUDE.md: guide for working in this repo

RentLens: an R Shiny app for exploring Singapore private-rental contracts (URA
data). It lives in **`local_shiny_app/`**, an R package called `rentalapp`.

- Live app: https://01a18a24-12d4-aa74-2e7f-2340d690bbe1.share.connect.posit.cloud/ (Posit Connect Cloud; every push to `main` redeploys it, so a broken `main` breaks the live site).
- People-facing docs: `README.md` (overview + **all data sources**) and
  `local_shiny_app/README.md` (package layout and how the modules talk).
- Keep both READMEs up to date when you add a feature, page or data source.

## Everyday commands

Run these from `local_shiny_app/`:

```r
source("dev/run_dev.R")                 # load the package and launch the app
source("dev/check.R")                   # roxygenise() + test_local() + writeManifest(); run before every commit
testthat::test_local(filter = "mod_settings")   # just one test file
```

Or from the shell:

```sh
cd local_shiny_app
Rscript -e 'roxygen2::roxygenise(); testthat::test_local()'
Rscript -e 'pkgload::load_all(); run_app(options = list(port = 4321))'   # app at http://127.0.0.1:4321
```

- **Headless check of the whole app.** This catches render errors that the unit
  tests miss:
  ```r
  testServer(run_app(), { session$setInputs(`settings-stored` = "none"); ... })
  ```
  Then read every output.
- **Commit hygiene.** Make sure the checks pass before you commit. Don't chain
  `check; git commit` with `;`, because that commits even when the checks fail.
  Commit only when the user asks.

## File map

### Repo root

| Path | What it is |
|------|------------|
| `README.md` | Overview of the app, the live link, and the **Data sources** table: where every data set comes from and the code that pulls it |
| `.gitignore` | Ignores `*.xlsx`; the raw URA Excel file is about 30 MB and never committed |

### `local_shiny_app/`: package plumbing

| Path | What it is |
|------|------------|
| `app.R` | Entry point (`shiny::runApp("local_shiny_app")`). It runs `pkgload::load_all()` and then `run_app()` |
| `DESCRIPTION` | Package metadata and the dependency list. Add new packages under `Imports:` |
| `manifest.json` | **Generated** by `rsconnect::writeManifest(appPrimaryDoc = "app.R")`. Lists the packages Posit Connect Cloud installs; the app redeploys from `main` on every push. Regenerate and commit it whenever a dependency changes (`dev/check.R` does it) |
| `.rscignore` | Folders left out of the deploy (`data-raw`, `dev`, `tests`, `man`), so their packages (readxl, testthat, ...) aren't installed on Connect Cloud |
| `NAMESPACE` | **Generated** by roxygen2 from `@import`/`@importFrom` tags. Never edit it by hand |
| `.Rbuildignore` | Files left out of the package build |
| `R/_disable_autoload.R` | Stops Shiny from sourcing `R/` itself; the package loader does it instead |
| `R/rentalapp-package.R` | Package-wide `@import shiny/bslib/leaflet` and other `@importFrom` tags |
| `R/run_app.R` | `run_app()`, the only exported function. It enables URL bookmarking |
| `R/app_config.R` | `app_sys()` builds paths into `inst/`. `app_theme()` is the bslib theme, plus `inst/app/rentlens.scss` |
| `R/app_ui.R` | The navbar and its pages, listed below. The **Search** page has `value = "budget"` (an old name, kept for links and `nav_select`) |
| `R/app_server.R` | Wires the modules together. It creates the shared `settings`, `filters`, `shortlist`, map selection and saved areas |

Pages in `R/app_ui.R`:
- Search
- Map
- Check a listing
- Market, with four sub-tabs: Compare areas, Area A vs B, Compare properties, Value for money
- Shortlist
- My filters (the code calls them settings)
- More → Rental records / About

### `local_shiny_app/R/`: logic (`fct_*`): no Shiny inputs or outputs, easy to unit-test

| File | Main contents |
|------|---------------|
| `fct_data.R` | `rentals_data()` loads `inst/extdata/rentals.rds` once per session, **drops the 1,168 contracts with no floor area** (landed houses listed as "<=1000"/">8000" sqft), adds district names and the nearest MRT. `filter_market()` is the single filter used everywhere: districts, beds, types, area, period, near-station, MRT distance, places and `place_km`. Constants: `bed_levels`, `area_max` (4000 = "4,000 sq ft and above") |
| `fct_rent.R` | Rent summaries: `latest_months()`, `summarise_rent()`, `summarise_options()`, `budget_verdict()`, `fits_budget()`, `summarise_projects()`, `compare_two_areas()`, `empty_shortlist()` |
| `fct_location.R` | Distances: `haversine_km()`, `mrt_stations()`, `schools_data()`, `nearest_mrt()`, `station_location()`, `walk_minutes()` (80 m per minute), `add_distance()` (one `To <place> (km)` column per place, named by `place_column()`). `destination_groups`/`key_places` are the hand-typed key places. `mrt_max` (2000 = "any distance") |
| `fct_listing.R` | "Check a listing": `onemap_search()` (OneMap API, cached per session), `comparable_sets()`, `asking_verdict()`, `district_region` |
| `fct_map.R` | Leaflet building blocks: `base_map()`, `map_palette()`, `add_key_places()`, `add_station_layers()`, `update_my_places()` (purple pins), `update_station_ring()`, `summarise_locations()`, draw toolbar and `drawn_to_sf()` |
| `fct_settings.R` | The **My filters** model (called settings in the code). `default_settings()`, `settings_to_json()`/`settings_from_json()` (checks every field and falls back to its default), `settings_summary()` (one capitalised chip per filter). Places (`new_places()`, `place_kinds` (no Home: old Home places load as Other, labelled Home), `chosen_place()`, `place_address()`, `place_label()`, `title_case_place()`, which keeps acronyms like NUS in capitals; `settings.js` has a copy of its word lists). `recent_choices` (6 months to 5 years), `period_for()`, `place_km_max` (30 = "any"), `max_places`, `settings_dependency()` (loads `settings.js`) |

### `local_shiny_app/R/`: modules (`mod_*`), each with `mod_x_ui(id)` and `mod_x_server(id, ...)`

| File | Page / role |
|------|-------------|
| `mod_settings.R` | **My filters** and the **first-visit setup wizard**. `settings_section_ui()` builds the five sections: budget, places, where to search, type of home, commute. The wizard shows them one step at a time; the dialog shows them as tabs. `optional_block()` makes the optional commute settings, greyed out until used (with a "Clear" link). `read_settings_form()` turns inputs into settings. Places are added one at a time and edited with the pencil, which loads one back into the form. The floor-area slider's handle labels show ft² and m²: `settings.js` swaps in its own `prettify` after Shiny creates or updates the slider (`.area-slider`), and `area_units()` writes the range under the label. The server returns `list(settings = reactiveVal, first_visit, save = function(s))` |
| `mod_filters.R` | Adapter from settings to data, plus the Search page's quick District/Project filters. It returns the reactives every other page uses: `base`, `market` (with the map selection applied), `selection`, `comparables`, `budget`, `places`, `summary`, ... |
| `mod_budget_check.R` | Search page: is my budget realistic? (verdict tiles and a histogram) |
| `mod_within_budget.R` | Search page: options within budget table, with distance columns. Its "Add to shortlist" button gets the wizard's closing tip |
| `mod_map.R` | Map page: building circles, heatmap, MRT/school/key-place layers, my places, click and draw selection, "Apply map selection to all pages", Save as Area A/B, "Only buildings within my budget" switch (on by default), "How to use the map" guide. Maps only Exact/Good geocodes (`well_located()` in `fct_map.R`). `new_map_selection()` |
| `mod_listing_check.R` | Check a listing: is an asking rent fair? Looks the address up with OneMap or takes a map pin. Flags verdicts based on fewer than `few_rentals` (5); `no_rentals_message()` in `fct_listing.R` |
| `mod_compare.R` | Market → Compare areas (districts vs budget). Charts grow with the number of districts; heatmap numbers are placed by position, because plotly reads "1", "2" on a category axis as positions |
| `mod_area_compare.R` | Market → Area A vs B (areas saved on the Map page), with the Singapore median for the same filters as a grey reference (needs `filters$settings`). `new_saved_areas()` |
| `mod_properties.R` | Market → Compare properties (up to three projects side by side) |
| `mod_value.R` | Market → Value for money (rent vs floor area, ranked by psf) |
| `mod_shortlist.R` | Shortlist page; `add_to_shortlist()` |
| `mod_transactions.R` | More → Rental records (raw table) |
| `mod_about.R` | More → About the data (static text, so no server). Update it when a page's behaviour changes |

### `local_shiny_app/R/`: helpers (`utils_*`)

| File | Main contents |
|------|---------------|
| `utils_format.R` | `money()`, `area_text()` (e.g. "500 - 1,200 sq ft (46 - 111 m²)"), `area_units()` ("500–1,200 ft² · 46–111 m²"), `sqft_per_sqm`, `quarter_label()`, `prettify()`, `format_money_cols()`, `listing_link()` |
| `utils_ui.R` | Shared UI: `rl_colours`, `kpi()`, `page_intro()`, `empty_state()`, `filter_box_ui()`/`filter_note_ui()`/`filter_chips()` (the "My filters" box at the top of each page), `more_options()`/`min_rentals_input()` (collapsed "More options"), `rl_plotly()`, `rl_datatable()` (its `search` box looks in the Project column only by default; `short_colnames()` keeps headers short), `rent_histogram()`, `range_dot_chart()`, `dot_sizes()` |
| `utils_bookmark.R` | `dt_bookmark_inputs()`: DataTable inputs to leave out of "Save search" links |

### `local_shiny_app/inst/`: files shipped with the app

| Path | What it is |
|------|------------|
| `inst/app/rentlens.scss` | Every custom style (settings summary, wizard, place list, `.optional-block` greying, etc.) |
| `inst/app/www/settings.js` | Browser side of My filters. See the next list |
| `inst/extdata/rentals.rds` | URA rental contracts, cleaned and geocoded. Rebuilt by `data-raw/rentals.R` |
| `inst/extdata/mrt_stations.csv` | 188 MRT/LRT stations (LTA exits averaged). Rebuilt by `data-raw/mrt_stations.R` |
| `inst/extdata/schools.csv` | MOE schools, geocoded with OneMap. Rebuilt by `data-raw/schools.R` |

What `inst/app/www/settings.js` does:
- Reads and writes `localStorage["rentlens-settings"]`.
- Sends `input$"settings-stored"`, which is `"none"` on a first visit.
- Handles the `rentlens-save-settings`, `rentlens-clear-settings` and `rentlens-coach` messages.
- Provides the OneMap autocomplete (`window.RentLens.searchPlaces`).
- Turns on a greyed optional block when you use it.
- Clears the "no scrolling" style Bootstrap can leave on `<body>` after the wizard's chained dialogs close.

### `local_shiny_app/data-raw/`, `dev/`, `tests/`

| Path | What it is |
|------|------------|
| `data-raw/rentals.R` | `data-raw/URA_Union_Cleaned_with_Address.xlsx` (git-ignored, put it there yourself) → `inst/extdata/rentals.rds`. Needs readxl |
| `data-raw/mrt_stations.R` | data.gov.sg LTA station exits → `mrt_stations.csv` |
| `data-raw/schools.R` | data.gov.sg MOE schools + OneMap geocoding → `schools.csv` (about a minute) |
| `dev/run_dev.R` | Development launcher |
| `dev/check.R` | `roxygenise()` + `test_local()` + `rsconnect::writeManifest()` |
| `tests/testthat/test-<file>.R` | One test file per `R/` file: unit tests for `fct_`/`utils_`, and `testServer()` tests for modules. `test-app_ui.R` renders the whole UI |
| `tests/testthat/helper-settings.R` | `fake_settings(...)`, a stand-in for `mod_settings_server()` in module tests |

## How to add things

- **A new page or tab:**
  1. Create `R/mod_x.R` with `mod_x_ui(id)` and `mod_x_server(id, filters)`.
  2. Add one line to `app_ui.R` and one to `app_server.R`.
  3. Add `tests/testthat/test-mod_x.R`.
  4. Mention the page in `mod_about.R` and the package README.
- **A new setting:**
  1. Add it to `default_settings()`.
  2. Validate it in `settings_from_json()`.
  3. Show it in `settings_summary()`.
  4. Add the input to `settings_section_ui()` and read it in `read_settings_form()`.
  5. Use it in `mod_filters_server()` / `filter_market()`.
  6. Exclude any helper inputs with `setBookmarkExclude()`.
- **A new data set:**
  1. Write the pull script in `data-raw/` and write its output to `inst/extdata/`.
  2. Add a loader in a `fct_` file.
  3. Add a row and code snippet to the root README's **Data sources**.
- **A new package function you call:** add an `@importFrom pkg fn` tag (or use `pkg::fn`), list the package in `DESCRIPTION`, then run `roxygen2::roxygenise()` and regenerate `manifest.json` (or the deployed app will fail to start).

## Gotchas (each one has bitten before)

- **Sentinel values mean "any".** The sliders don't show these values; "off" or "any" is stored as the sentinel:

  | Constant | Value | Means | Slider range shown |
  |---|---|---|---|
  | `mrt_max` | 2000 | any distance | 100–1,900 m |
  | `place_km_max` | 30 | any distance | 1–29 km |
  | `area_max` | 4000 | 4,000 sq ft and above | |

- **Settings live in the browser, not the server.** There are no accounts. To see the wizard again, use My filters → Start over, or run `localStorage.removeItem("rentlens-settings")` in the console. The period is always recomputed from `recent` and never stored on its own.
- **Shiny custom-message handlers must take exactly one argument.** A zero-argument handler throws when Shiny connects and breaks the whole app silently. Register them when `settings.js` loads, not inside `shiny:connected`.
- **Don't use `%||%`.** It needs R 4.4; write `if (is.null(x)) y else x`.
- **Don't use `modifyList()` on settings in tests.** It merges into the `places` data frame. Assign fields instead, as in `fake_settings()`.
- **`testServer()` gets its own copies of default arguments.** Pass reactive arguments such as areas and places explicitly.
- **Zero-length vectors.** Code must work with zero places. New users start with none, and `paste0()` needs `recycle0 = TRUE`; see `place_column()`.
- **The test data has quirks.** Some project names are generic ("LANDED HOUSING DEVELOPMENT"). There are no private rentals within 500 m of Kent Ridge MRT, so tests use Novena.
- **OneMap is live and keyless.** Server side, call `onemap_search()`; in the browser it's `RentLens.searchPlaces`. Tests should pass a fake `search =` function rather than hit the network.

## Style

- Comments and UI text are plain English for international renters: say "Search page", not "D10" or jargon. Match the comment density of the file you're editing.
- Use roxygen `#' ... @noRd` for internal functions. Only `run_app()` is exported.
- Keep logic in `fct_` (testable) and keep modules thin.
