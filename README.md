# forkingpaths

A p-hacking roguelike for the R console. Made for bored statisticians.

```r
# install.packages("remotes")
remotes::install_github("tajmk2/forkingpaths")
career <- forkingpaths::play()
```

You're a new assistant professor with three years, nine studies and three
hearts of credibility. Each study starts with a dataset and a preregistered
analysis. If p isn't below the journal's alpha, spend your **researcher
degrees of freedom** until it is:

| Move | DoF | Suspicion |
|---|---|---|
| Exclude outliers (3 / 2.5 / 2 SD) | 1 | 1 |
| Add or drop a covariate | 1 | 1 |
| Log-transform the outcome | 1 | 1 |
| Switch the outcome | 2 | 2 |
| Analyse a subgroup | 2 | 2 |
| Recruit 10 more participants | 2 | 1 |
| Go one-tailed | 2 | 3 |
| Return to the preregistered analysis | free | (paths stay explored) |

When you submit:

1. **Reviewer 2** may catch you. The chance goes up with suspicion, and getting caught costs 1 credibility.
2. If you're published, someone may **replicate** your exact analysis on 2.5x the data. A success restores 1 credibility; a failure costs 1.
3. Then you learn **the truth**. About 30% of studies have a real effect; the rest are noise.

Journals get stricter each year (alpha .05, then .01, then .005). After each
year you pick one of three perks. If you hit zero credibility you're fired. Tenure needs 600 points.

While you play, the Plots pane shows your lab at night in pixel art. The CRT
shows the current p and turns amber when it's significant, the file-drawer
pile grows, and Reviewer 2 creeps closer as your suspicion rises. Below the
scene are the two-group data and your trail of p-values.

When the game ends you get your career back as a list, with the true effect
behind every study:

```r
career$history
```

## Autopilot: write a strategy, simulate a thousand careers

A strategy is a function that takes the current `state` and returns
`"submit"`, `"file_drawer"`, a move id from `state$moves`, or a move type
such as `"more_data"`. `peek(state)` shows the p-value every move would give
without spending anything. (Recruiting gives `NA`, because those
participants don't exist yet.)

```r
library(forkingpaths)

my_strategy <- function(state) {
  if (state$can_submit && state$suspicion <= 3) return("submit")
  options <- peek(state)
  options <- options[!is.na(options$p_after) & options$type != "reset", ]
  if (nrow(options) == 0 || state$suspicion > 3) return("file_drawer")
  options$id[which.min(options$p_after)]
}

res <- autopilot(my_strategy, n_careers = 500, seed = 1)
res
plot(res)

cmp <- compare_strategies(honest = strat_honest, greedy = strat_greedy,
                          cautious = strat_cautious,
                          stopping = strat_optional_stopping,
                          mine = my_strategy, n_careers = 500)
cmp
plot(cmp)   # tenure rate vs false-positive share: the incentive landscape
```

Built-in strategies, from 500 careers each:

| Strategy | Tenure | Fired | Papers | False positives | Replicates |
|---|---|---|---|---|---|
| `strat_honest` | 14% | 0% | 0.6 | 20% | 63% |
| `strat_optional_stopping` | 29% | 0% | 1.3 | 20% | 61% |
| `strat_cautious` (suspicion <= 4) | 43% | 5% | 2.4 | 47% | 39% |
| `strat_greedy` | 46% | 12% | 2.9 | 49% | 31% |

That is the point. Can you beat 46% tenure with fewer than 30% false positives?

Other knobs: `autopilot(perk_strategy = function(offer, perks) ...)` picks
perks (ids in `perk_list()`), and `max_steps` stops strategies that stall.

## Options

- `play(seed = 1)` replays a career.
- `play(plot = FALSE)` skips the plots.
- `options(forkingpaths.scene = FALSE)` swaps the pixel-art lab for plain light-theme plots.
- `options(forkingpaths.delay = 0)` turns off the dramatic pauses.
- `options(forkingpaths.utf8 = FALSE)` forces ASCII glyphs.
