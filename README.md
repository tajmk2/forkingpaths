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

When the game ends you get your career back as a list, with the true effect
behind every study:

```r
career$history
```

## Balance notes

A greedy bot that hacks until significant gets tenure in ~49% of careers,
is fired in ~18%, and 54% of its published papers are false positives. An
honest bot is never fired, but gets tenure in only ~16%. That is the point.

## Options

- `play(seed = 1)` replays a career.
- `play(plot = FALSE)` skips the plots.
- `options(forkingpaths.delay = 0)` turns off the dramatic pauses.
- `options(forkingpaths.utf8 = FALSE)` forces ASCII glyphs.
