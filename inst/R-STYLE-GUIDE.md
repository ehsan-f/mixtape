# R Code Style Guide

A record of the formatting conventions used in this project's R code, so they can be
applied consistently to other projects.

## Comment Headings

Four levels of section headings, by scope:

```r
##### Section Title #####
#----- Sub Heading
#-- Sub-sub heading
#- Sub-sub-sub heading
```

- `#####` wraps the title on both sides (top-level sections, e.g. `Packages`, `Config`, `Load Data`, `Export`).
- `#-----`, `#--`, and `#-` only prefix the title (no closing dashes), one space before the text.
- Use the level that matches how deeply nested the step is inside a section.

## Pipes and Assignment

- Use the native pipe `|>`, not `%>%`.
- Use `<-` for assignment, `=` only for naming function arguments.

## tidyverse

- If a `select()` / `mutate()` / `summarise()` / `reframe()` / `filter()` call involves only
  **one** variable/argument, keep it on the same line as the call:
  ```r
  select(user_id)
  filter(total_monthly_debit_vol_current > 0)
  ```
- If **more than one** is involved, always break onto new lines, one per variable/argument,
  indented one level from the call, with the closing `)` on its own line:
  ```r
  select(
    user_id,
    month,
    serene_tag_final
  )

  mutate(
    set_to_zero_account = coalesce(set_to_zero_account, 0),
    set_to_zero_user = coalesce(set_to_zero_user, 0),
    cc_payment_set_to_zero = pmax(set_to_zero_account, set_to_zero_user)
  )
  ```
- `summarise()` calls that group data should include `.groups = 'drop'` as the final argument.
- `group_by()` always stays on a single line, regardless of how many variables are grouped:
  ```r
  group_by(user_id, month, serene_tag_final, account_type) |>
  ```

## data.table

The same readability rules as the tidyverse section, applied to `dt[i, j, by]`.

- Use `.()` rather than `list()` in `j` and `by`.
- Always name `by =` explicitly, never pass it positionally.
- If the call only does **one** thing (one filter, one new column, one summary), keep it on one line:
  ```r
  dt[amount > 0]
  dt[, total_amount := debit_amount + credit_amount]
  dt[, .(total_amount = sum(amount)), by = .(user_id, month)]
  ```
- If `j` creates or summarises **more than one** column, break it onto new lines, one column per
  line, with the closing `)` on its own line (the same as `mutate()` / `summarise()`):
  ```r
  dt[, `:=`(
    set_to_zero_account = fcoalesce(set_to_zero_account, 0),
    set_to_zero_user = fcoalesce(set_to_zero_user, 0)
  )]

  dt[, .(
    total_amount = sum(amount),
    n_transactions = .N
  ), by = .(user_id, month)]
  ```
- `by = .(...)` always stays on a single line, like `group_by()`.
- If `i`, `j` and `by` are all used and the call no longer reads easily on one line, put each on
  its own line, indented one level, with the closing `]` on its own line:
  ```r
  dt[
    amount > 0,
    .(total_amount = sum(amount)),
    by = .(user_id, month)
  ]
  ```
- Avoid long `][` chains. Chain at most two steps; beyond that, assign an intermediate result.
- `:=` modifies the table in place (by reference). Inside a function, call `copy()` first if
  the caller's table must not change.

## No Alignment Padding

- Never pad/align `=` signs, argument names, or values into columns. Arguments are written
  with normal single-space spacing only — no vertical alignment.

## Quotes

- Use single quotes (`'...'`) for strings by default.
- Use double quotes only when the string itself needs to contain a single quote/apostrophe
  (e.g. `"don't"`), to avoid escaping.

## Booleans

- Use `T` / `F`, not `TRUE` / `FALSE`.

## Variable Naming Prefixes

Hungarian-style prefixes indicate what a variable holds:

| Prefix | Meaning | Usage |
|---|---|---|
| `ds_` | dataset/dataframe | assigned in the global environment, or inside an API route handler |
| `df_` | dataframe | function argument/local dataframe within a function |
| `ls_` | list | e.g. `ls_config` |
| `v_`  | single value or vector | e.g. `v_time_taken`, `v_prefix` |

data.table objects use the same `ds_` / `df_` prefixes as dataframes.

## Function Naming

- Functions are named with a verb prefix in `snake_case`: `get_...`, `add_...`, `apply_...`,
  `build_...`, `clean_...`, `connect_...`, `extract_...`.

## Indentation

- 2 spaces per indent level (pipe continuations, nested calls, function bodies). No tabs.
