# RentLens: product requirements and feature review

This document records what each feature of RentLens does, how it works, and whether it is still worth keeping. Each feature ends with a **verdict**:

- **Keep:** clearly useful.
- **Trim:** useful, but parts of it duplicate something else.
- **Merge:** better folded into another feature.
- **Cut:** not earning its place.

The verdicts are proposals. Tick your decisions in the [decision table](#7-decision-table) at the end.

Keep this file up to date when a feature is added, changed or removed.

## 1. Product summary

**Who it is for.** People renting a private home in Singapore, especially newcomers who don't know the districts. Many are international students and professionals.

**The problem.** Asking rents on listing sites are hard to judge. Renters want to know:

- "Is my budget realistic?"
- "Where can I afford to live near my office or school?"
- "Is this asking rent fair?"

**What RentLens does.** It answers those questions from real signed contracts. These are URA's private residential rental contracts, about 411,000 of them, from Sep 2021 to Aug 2026. The answers follow the renter's own budget, places that matter, type of home and commute.

**Out of scope:**

- live listings
- HDB flats (URA covers private homes only)
- accounts (filters are saved in the browser)

## 2. Main journey

1. **First visit: setup wizard.** It has five steps:
   1. budget
   2. places that matter
   3. where to search
   4. type of home
   5. commute

   The answers become **My filters**.
2. **Search.** Shows whether the budget is realistic, then the projects and bedroom types that fit it.
3. **Map.** Shows where those homes are, near the renter's places and the MRT.
4. **Check a listing.** Judges a specific advert against past rents nearby.
5. **Shortlist.** Collects options from Search, Map, Check a listing and Compare properties, and compares them.
6. **Market.** Deeper comparisons for renters who want them.

## 3. Features

Each entry covers:

- **Does:** the renter's view.
- **How:** the code involved, so you know what you'd be removing.
- **Verdict.**

### 3.1 My filters and the setup wizard (`mod_settings.R`, `fct_settings.R`, `settings.js`)

The **My filters** model has five sections. The wizard shows them one step at a time on a first visit; the "My filters" dialog shows them as tabs.

| Section | Does | How |
|---|---|---|
| Budget | Monthly range S$0 – S$15,000, with a hint showing what share of the last 12 months' rentals fall in it | `sliderInput("budget")`, `budget_hint` |
| Places that matter | Add places one at a time: search an address (OneMap), pick an icon (Work, Study, ... or Other with a custom label), and name it. Edit a place with its pencil | `place_search` uses `RentLens.searchPlaces` in `settings.js`, then `chosen_place()`; `new_places()`, `place_kinds` |
| Where to search | Districts, and how recent the rentals are (6 months to 5 years) | `districts`, `recent` → `period_for()` |
| Type of home | Bedrooms (1–5+), property type, and floor area (slider plus exact Lower/Upper limits in ft² or m²) | `beds` (`bed_choices`), `types`, `area` with `area_lo`/`area_hi`; `settings.js` keeps them in step |
| Commute (optional, greyed out until used) | Within N m of any MRT/LRT; near one chosen station; within N km of my places | `optional_block()`; sentinels `mrt_max`, `place_km_max` |

Other parts:

- **Storage.** Saved in `localStorage["rentlens-settings"]` as versioned JSON (`settings_to_json()`, now version 2). `settings_from_json()` checks every field and falls back to its default.
- **Summary box.** "MY FILTERS" chips with an "Edit my filters" button, shown on every page (`settings_summary()`).
- **Start over.** Clears the saved filters and reopens the wizard.

**Verdict: Keep.** Every other page depends on it. One thing to watch: it is the largest module (500 lines), so new settings should go into `fct_settings.R` where possible.

### 3.2 Search page

| Feature | Does | How | Verdict |
|---|---|---|---|
| Quick District / Project filters | Narrow the page without opening My filters. Districts stay in step with My filters | `mod_filters_ui`. Projects are not saved; they apply to `filters$selection()` only | **Keep** |
| Save search | A link that restores filters, shortlist and map selection | Shiny URL bookmarking, `bookmarkButton()` and `onBookmark` in the map and shortlist | **Keep**, but it is little known: "Save search" sounds like saving to an account. Consider renaming it "Copy link to this search" |
| Budget verdict | "Your budget is realistic / below typical / above typical", with the % of comparable rentals in range and advice | `budget_verdict()` in `fct_rent.R` | **Keep** |
| KPI tiles | Median rent, typical range (middle 50%), number of comparable rentals | `mod_budget_check` | **Keep** |
| Budget histogram | Recent rents with the budget band and the median line | `rent_histogram()` in `utils_ui.R` | **Keep** |
| Options within budget: List | One row per project and bedroom type whose recent median rent is in the budget, with distance columns to each place | `summarise_options()` + `add_distance()`; "Min. recent rentals" under More options | **Keep**. This is the core answer |
| Options within budget: By district | Stacked bars counting options per district and bedroom | `mod_within_budget` `plot` | **Trim?** Market → Compare areas already compares districts. Useful as a quick overview, but you could cut it |

### 3.3 Map page (`mod_map.R`, `fct_map.R`)

| Feature | Does | How | Verdict |
|---|---|---|---|
| Building circles | One circle per mapped building. Size = rentals; colour = median rent vs budget (or median rent / psf) | `summarise_locations()`, `map_palette()`. Only Exact/Good geocodes are mapped (`well_located()`) | **Keep** |
| Only buildings within my budget | Hides buildings whose median rent is outside the budget (on by default) | `map_data()` filters by the median per `loc_id` | **Keep** |
| Layers and map key | Buildings and my places (purple pins) show by default. Universities and polytechnics (stars), work areas, hospitals, airport and ports, MRT/LRT stations, schools and the heatmap start hidden and are turned on with the layers button; Light, OneMap and Satellite basemaps. A "Map key" box (top right) shows every symbol; the colour scale (bottom right) uses one colour, light = cheap, dark = dear. The side panel says how many filtered rentals have no exact location | `key_layers`, `add_key_places()`, `map_key()`, `add_station_layers()`, `update_my_places()`, `map_palette()` | **Keep**, possibly with fewer layers: check whether the heatmap and schools are used |
| Station ring | A ring around the station picked in My filters, and the map flies to it | `update_station_ring()` | **Keep** |
| Click / draw selection | Click one building or draw a shape; the side panel and everything below follow it | `map_marker_click`, `map_draw_new_feature` → `map_sel` | **Keep** |
| Apply map selection to all pages | Narrows Search, Market, Shortlist and Rental records to the selection | `map_sel$apply` → `filters$market()` | **Keep**. Shown as a chip so people know it is on |
| Saved areas | 'Save this area' adds the selection to a list (up to 4) in the side panel, each with its own colour (map outline), an automatic name (the project, or 'Around <station> · N buildings'), ✎ rename and × remove. Kept in the browser and in Save search links | `new_saved_areas()`, `add_area()`, `name_area()`, `areas_to_json()` in `fct_map.R`; `settings.js` | **Keep** |
| Compare saved areas | Card under the map, from 2 saved areas: rentals, projects, median and typical rent, share within budget, psf, walk to the nearest MRT/LRT, main district and distance to each of my places, plus rent by bedrooms and by quarter, with Singapore as a grey reference. Uses every rental in the areas' buildings, whatever the within-budget switch | `mod_saved_areas.R`; `compare_areas()`, `compare_areas_by_bed()` in `fct_rent.R` | **Keep** |
| Clear selection | Clears the selection; saved areas stay | `input$clear` | **Keep** (fixed: it used to wipe the saved areas too) |
| How to use the map | A 5-step guide in a dialog | `input$help` | **Keep** |
| Side KPIs | Rentals, projects, median rent and median psf of the selection | `vb_*` | **Keep** |
| Psf trend by month | Median psf per month, per bedroom and overall | `output$trend` | **Trim?** Compare saved areas and Compare properties show trends too; this one is the noisiest (monthly) |
| By bedrooms table | Rentals, median rent, sqft and psf per bedroom for the selection | `output$beds` | **Keep** (small and useful) |
| Projects in the selection | Project and bedroom rows with add to shortlist | `table_data()` → `summarise_options()` | **Keep** |
| Rentals in the selection | Raw contracts with a CSV download | `contracts()` | **Merge** into More → Rental records, which shows the same contracts (and follows the map selection when it is applied). Removing it shortens the page |

### 3.4 Check a listing (`mod_listing_check.R`, `fct_listing.R`)

| Feature | Does | How | Verdict |
|---|---|---|---|
| Address lookup | Type an address, postal code or building, or click an example chip. Several matches give a picker | `onemap_search()`, cached per session | **Keep** |
| Map pin | Click the map to place the listing instead | `input$map_click` | **Keep** |
| Listing details | Asking rent, floor area, bedrooms (Any or 1–5+), type, and the "Nearby" radius (200–2,000 m) | inputs | **Keep** |
| Comparable levels | Same project, Within N m, Same district, Same region | `comparable_sets()` | **Keep** |
| Verdict | "Asking rent is high / fair / low" against the chosen level; a few-data flag below 5 rentals; a message when nothing matches | `asking_verdict()`, `few_rentals`, `no_rentals_message()` | **Keep** |
| Comparables table, chart and map | Past rentals at each level, a chart with the asking rent, and dark circles for the comparable buildings | `output$table`, `output$plot`, map | **Keep** |
| Add to shortlist | Adds the matched project with the chosen bedrooms | `add_to_shortlist()` | **Keep** |

### 3.5 Market (`mod_compare.R`, `mod_properties.R`, `mod_value.R`)

| Tab | Does | How | Verdict |
|---|---|---|---|
| Compare areas: district/project chart and table | Median and typical range per district (or per project), against the budget band. Share within budget is highlighted | `range_dot_chart()` | **Keep** |
| Compare areas: district × bedroom heatmap | Median rent grid, soft green to red | `heat_data()` | **Keep** |
| Compare areas: MRT premium | "Does being near an MRT/LRT station cost more?": psf vs distance per project, with a table | `nearest_mrt()` | **Keep?** Interesting, but not what a renter decides on. A candidate to **Cut** if the page feels long |
| Compare properties | Up to 3 projects side by side: cards, rent box plot, quarterly trend, table, "From shortlist", download | `mod_properties` | **Keep** |
| Value for money | Median rent vs median floor area per project, and a table ranked by psf | `mod_value` | **Keep**. It is the only view that ranks by space for the money |

### 3.6 Shortlist (`mod_shortlist.R`)

| Feature | Does | Verdict |
|---|---|---|
| Table | Each shortlisted project and bedroom with median rent, range and distances; remove, clear, download | **Keep** |
| At a glance | Median rent and typical range per option against the budget | **Keep** |
| How their rents have moved | Quarterly median per option | **Keep** (the only trend left after Price trends was removed) |
| Navbar badge | Count next to "Shortlist" | **Keep** |

The shortlist is kept only for the session and in Save search links; it is **not** saved in the browser like My filters. Consider saving it in localStorage too, so it survives a reload.

### 3.7 More

| Page | Does | Verdict |
|---|---|---|
| Rental records | Every contract matching the filters (and the map selection), newest first, with a CSV download | **Keep**. It becomes the single raw-data table if the Map's contracts table is merged into it |
| About the data | Sources, how figures are worked out, limitations | **Keep**. Update it when behaviour changes |

## 4. Shared plumbing (no verdict needed)

- **`filter_market()`** (`fct_data.R`). The single filter behind every page: districts, bedrooms, types, floor area, period, station, MRT distance and places.
- **`mod_filters_server()`.** Turns settings into the reactives every page reads:
  - `base`: all the filters
  - `market`: plus the map selection
  - `selection`: plus the projects
  - `comparables`: the latest months only
- **Shortlist.** A data frame of `project` and `bed_group`, shared through `app_server.R`.
- **OneMap search.** In R (`onemap_search()`) for Check a listing, and in the browser (`searchPlaces()`) for places.
- **Bookmarking.** Every helper input is left out with `setBookmarkExclude()`.

## 5. Data (`inst/extdata/`)

| File | Rows | Columns | Built by |
|---|---|---|---|
| `rentals.rds` | 411,427 contracts | project, street, district_no, type, bedrooms, rent, area_band, area_sqft, psf, lease_date, address, lat, lng, geo_match | `data-raw/rentals.R` |
| `mrt_stations.csv` | 188 stations (41 LRT) | station, lat, lng, kind | `data-raw/mrt_stations.R` |
| `schools.csv` | 337 schools | name, level, postal, lat, lng | `data-raw/schools.R` |

All columns are used: `street`, `area_band` and `geo_match` appear in the records tables, and `postal` identifies a school.

Rows that are left out or limited:

- **No floor area (1,168).** Landed houses URA lists only as "<=1000" or ">8000" sqft. These are dropped when `rentals.rds` is built.
- **No bedroom count (54,481).** Every landed house, plus some flats. They are kept, but show only when every bedroom choice is picked ("Any bedrooms").
- **No location (1,581).** Never on the map and never in distance filters. Only Exact and Good geocodes are mapped (`well_located()`). Street-level geocodes are also left out of Check a listing's radius.

## 6. Known limitations

- Psf uses the midpoint of URA's floor-area band, so it is an estimate.
- Distances are straight lines; walking time assumes 80 m a minute.
- My filters are per browser; the shortlist is per session.
- The theme's CSS address doesn't change between deploys, so a returning visitor can see old styles until a hard refresh.
- OneMap is called live: if it is down, place search and Check a listing lookups fail, but the rest of the app still works.

## 7. Decision table

Tick one per row.

| Feature | Proposed | Keep | Trim | Merge | Cut |
|---|---|:-:|:-:|:-:|:-:|
| Search → By district chart | Trim? | ☐ | ☐ | ☐ | ☐ |
| "Save search" wording | Rename | ☐ | ☐ | ☐ | ☐ |
| Map → psf trend by month | Trim? | ☐ | ☐ | ☐ | ☐ |
| Map → Rentals in the selection | Merge into Rental records | ☐ | ☐ | ☐ | ☐ |
| Map → heatmap / schools / hospitals / airport layers | Keep? | ☐ | ☐ | ☐ | ☐ |
| Market → MRT premium chart and table | Keep? | ☐ | ☐ | ☐ | ☐ |
| Shortlist saved in the browser | Add | ☐ | ☐ | ☐ | ☐ |
| Everything else in section 3 | Keep | ☐ | ☐ | ☐ | ☐ |
