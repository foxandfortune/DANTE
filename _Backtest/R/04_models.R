# Model specifications -----------------------------------------------------------
# A model is a list with
#   fit(train, ctx)          -> state  (train = team-game rows before the date)
#   predict(state, games)    -> data.frame(game_id, pace, mu_home, mu_away,
#                                          sd_margin, sd_total)   (points)
# ctx carries league, season, priors, a lambda cache and the as-of date.

# Lambda cache: re-run CV only every `cv_every` days per stat (applied to every
# model alike) to keep a daily walk-forward affordable
cached_ridge <- function(ctx, key, train, stat, feats, wgt, ...) {
  lc <- ctx$lambda_cache
  hit <- lc[[key]]
  if (!is.null(hit) && as.numeric(ctx$as_of - hit$date) < ctx$cv_every) {
    return(fit_ridge(train, stat, feats, wgt, fixed_lambda = hit$lambda, ...))
  }
  r <- fit_ridge(train, stat, feats, wgt, ...)
  lc[[key]] <- list(date = ctx$as_of, lambda = attr(r, "lambda"))
  r
}

coef_vec <- function(cf) setNames(cf$value, cf$name)
lookup <- function(v, nm) { x <- v[nm]; x[is.na(x)] <- NA_real_; unname(x) }

# Off/Def pair for a component model (Off includes intercept), as process_games
comp_pred <- function(v, off_id, def_id) {
  v["(Intercept)"] + lookup(v, paste0("team_id_", off_id)) +
    lookup(v, paste0("opp_id_", def_id))
}

# ---- 1. Production baseline (component pathway) -------------------------------
#  opts: priors (TRUE/FALSE), stretch (1.3225 replicates the sequential mutate,
#  1 = none), sigma (pts/100 per team, independent), standardize, fold_by_game,
#  lambda_mult (NULL = production 1+lambda_adj), situational_components
model_components <- function(priors = TRUE, stretch = "prod", sigma = 13,
                             standardize = TRUE, fold_by_game = FALSE,
                             lambda_mult = NULL, situational_components = FALSE,
                             netlevel_ratio = NULL, prior_mode = "blend",
                             prior_lambda_mult = 1, blend_override = NULL,
                             label = "prod_components") {
  # prior_mode: "blend" = production (fit shrinks to 0, then mix with the prior on
  #   the games-played curve); "offset" = the ridge shrinks toward the prior instead
  #   of toward 0 (lambda x prior_lambda_mult), so the data take over as games
  #   accumulate with no separate curve. Intercept/covariates are then the current
  #   fit's.
  # blend_override: named list by component (pace, ast, oreb, to, rtg) of
  #   list(prior_wgt, prior_min, max_games) replacing the with_priors weights
  # lambda_mult: NULL = production (1 + lambda_adj); a single number for every
  #   component; or a named list/vector by component (pace, ast, oreb, to, rtg),
  #   missing names keep production
  # netlevel_ratio: if set, the rtg model uses the net/level parameterization
  #   with the level part penalized netlevel_ratio x as hard
  sit <- c("is_home", "neutral_site", "days_rest", "travel")
  list(
    label = label,
    fit = function(train, ctx) {
      gp <- games_played_tbl(train)
      out <- list()
      for (nm in names(prod_stats)) {
        st <- prod_stats[[nm]]
        w <- load_weights(ctx$league, st, ctx$season)
        feats <- prod_features[[st]]
        if (situational_components && nm != "rtg") feats <- c(feats, sit)
        lm <- if (is.null(lambda_mult)) NULL else if (length(lambda_mult) == 1 && is.null(names(lambda_mult)))
          lambda_mult else lambda_mult[[nm]]
        if (is.null(lm) || (length(lm) == 1 && is.na(lm))) lm <- 1 + w$single$lambda_adj
        nl <- if (nm == "rtg") netlevel_ratio else NULL
        use_prior <- priors && !is.null(ctx$priors)
        offs <- NULL
        if (use_prior && prior_mode == "offset") {
          pr <- ctx$priors[[nm]]; pr <- pr[grepl("^(team|opp)_id_", pr$name), ]
          offs <- setNames(pr$value, pr$name); lm <- lm * prior_lambda_mult
        }
        cf <- cached_ridge(ctx, paste(label, nm), train, st, feats, w$single$wgt,
                           lambda_mult = lm, standardize = standardize,
                           fold_by_game = fold_by_game, netlevel_ratio = nl,
                           offset_prior = offs)
        if (use_prior && prior_mode == "blend") {
          wp <- w$prior
          if (!is.null(blend_override[[nm]])) for (k in names(blend_override[[nm]])) wp[[k]] <- blend_override[[nm]][[k]]
          cf <- blend_with_prior(cf, ctx$priors[[nm]], gp, wp)
        }
        out[[nm]] <- coef_vec(cf)
      }
      out
    },
    predict = function(state, g) {
      r <- state
      sitv <- function(v, home, rest, dist, neutral) {
        z <- function(n) ifelse(is.na(v[n]), 0, v[n])
        z("is_home") * home * (1 - neutral) + z("neutral_site") * neutral +
          z("days_rest") * rest + z("travel") * dist
      }
      neu <- as.numeric(g$neutral)
      pace <- (comp_pred(r$pace, g$home_id, g$away_id) +
               comp_pred(r$pace, g$away_id, g$home_id)) / 2
      cp <- function(nm, o, d, home, rest, dist) {
        x <- comp_pred(r[[nm]], o, d)
        if (situational_components) x <- x + sitv(r[[nm]], home, rest, dist, neu)
        x
      }
      v <- r$rtg
      base <- function(o, d, home, rest, dist) {
        v["(Intercept)"] + lookup(v, paste0("team_id_", o)) +
          lookup(v, paste0("opp_id_", d)) + v["poss_per_40"] * pace +
          v["oreb_rt"] * cp("oreb", o, d, home, rest, dist) +
          v["to_rt"]   * cp("to",   o, d, home, rest, dist) +
          v["ast_rt"]  * cp("ast",  o, d, home, rest, dist) +
          sitv(v, home, rest, dist, neu)
      }
      h <- base(g$home_id, g$away_id, 1, g$home_rest, g$home_dist)
      a <- base(g$away_id, g$home_id, 0, g$away_rest, g$away_dist)
      if (identical(stretch, "prod")) {
        # exact production order of operations (sequential mutate)
        a2 <- a + 0.5 * 0.3 * (a - h); h2 <- h + 0.5 * 0.3 * (h - a2)
      } else {
        m <- (h + a) / 2; h2 <- m + stretch * (h - m); a2 <- m + stretch * (a - m)
      }
      data.frame(game_id = g$game_id, pace = unname(pace),
                 mu_home = unname(h2 * pace / 100), mu_away = unname(a2 * pace / 100),
                 sd_margin = sigma * sqrt(2) * unname(pace) / 100,
                 sd_total = sigma * sqrt(2) * unname(pace) / 100)
    }
  )
}

# ---- 2. Direct efficiency model ------------------------------------------------
# Efficiency regressed directly on team offense + opponent defense + predicted
# pace + situational terms (the site's `raw_rating` spec). No realized
# component stats, so the team coefficient is the "all-in" rating and HFA is not
# partialled out. Pace has its own model (optionally with situational terms).
#   rtg_wgt / pace_wgt: recency decay (NULL = production single-season value)
model_direct <- function(priors = TRUE, stretch = 1, sigma = 13,
                         standardize = TRUE, fold_by_game = FALSE,
                         lambda_mult = NULL, situational_pace = FALSE,
                         rtg_wgt = NULL, pace_wgt = NULL, label = "direct") {
  sit <- c("is_home", "neutral_site", "days_rest", "travel")
  rtg_feats  <- c("game_date", "team_id", "opp_id", "poss_per_40", sit)
  pace_feats <- c("game_date", "team_id", "opp_id", if (situational_pace) sit)
  list(
    label = label,
    prior_state = function(pr) list(pace = coef_vec(pr$pace), eff = coef_vec(pr$raw_rating)),
    fit = function(train, ctx) {
      gp <- games_played_tbl(train)
      one <- function(key, stat, feats, wgt_override, prior_name) {
        w <- load_weights(ctx$league, stat, ctx$season)
        lm <- if (is.null(lambda_mult)) 1 + w$single$lambda_adj else lambda_mult
        wg <- if (is.null(wgt_override)) w$single$wgt else wgt_override
        cf <- cached_ridge(ctx, paste(label, key), train, stat, feats, wg,
                           lambda_mult = lm, standardize = standardize,
                           fold_by_game = fold_by_game)
        if (priors && !is.null(ctx$priors))
          cf <- blend_with_prior(cf, ctx$priors[[prior_name]], gp, w$prior)
        coef_vec(cf)
      }
      list(pace = one("pace", "poss_per_40", pace_feats, pace_wgt, "pace"),
           eff  = one("eff", "rtg", rtg_feats, rtg_wgt, "raw_rating"))
    },
    predict = function(state, g) {
      neu <- as.numeric(g$neutral)
      sitv <- function(v, home, rest, dist) {
        z <- function(n) ifelse(is.na(v[n]), 0, v[n])
        z("is_home") * home * (1 - neu) + z("neutral_site") * neu +
          z("days_rest") * rest + z("travel") * dist
      }
      pv <- state$pace
      ph <- comp_pred(pv, g$home_id, g$away_id); pa <- comp_pred(pv, g$away_id, g$home_id)
      if (situational_pace) {
        ph <- ph + sitv(pv, 1, g$home_rest, g$home_dist)
        pa <- pa + sitv(pv, 0, g$away_rest, g$away_dist)
      }
      pace <- (ph + pa) / 2
      v <- state$eff
      base <- function(o, d, home, rest, dist)
        v["(Intercept)"] + lookup(v, paste0("team_id_", o)) +
        lookup(v, paste0("opp_id_", d)) + v["poss_per_40"] * pace + sitv(v, home, rest, dist)
      h <- base(g$home_id, g$away_id, 1, g$home_rest, g$home_dist)
      a <- base(g$away_id, g$home_id, 0, g$away_rest, g$away_dist)
      m <- (h + a) / 2; h2 <- m + stretch * (h - m); a2 <- m + stretch * (a - m)
      data.frame(game_id = g$game_id, pace = unname(pace),
                 mu_home = unname(h2 * pace / 100), mu_away = unname(a2 * pace / 100),
                 sd_margin = sigma * sqrt(2) * unname(pace) / 100,
                 sd_total = sigma * sqrt(2) * unname(pace) / 100)
    }
  )
}

# ---- 3. Expected rating + team shooting (x-model) ------------------------------
# Splits the efficiency model in two:
#   xrtg : expected points/100 from shot quality (x_rtg), same features and
#          production settings as the rtg model (team, opp, realized pace/TO/
#          OREB/AST, situational), so the component pathway is unchanged
#   soe  : shooting over expected = rtg - x_rtg, a team-specific (offense and
#          defense) shooting number with its own recency decay and shrinkage
# predicted efficiency = xrtg prediction + soe prediction.
# Rows without x_rtg (box-score fallback games) are dropped for these two fits.
# Priors: no x_rtg / soe priors exist; when production priors are used, the
# xrtg team/opp coefficients are blended with the rtg priors (team deviations,
# intercept and covariates come from the current fit) and soe has no prior.
model_xshoot <- function(priors = TRUE, soe_wgt = 0.99, soe_lambda_mult = 2,
                         soe_situational = FALSE, impute_missing = FALSE,
                         label = "X_xshoot") {
  # impute_missing = TRUE keeps games without x_rtg (box-score fallback) in the
  # expected-rating fit by using their actual rtg there (shooting-over-expected
  # = 0 for those games); the soe fit still uses only games with x_rtg.
  sit <- c("is_home", "neutral_site", "days_rest", "travel")
  comp <- model_components(priors = priors, stretch = 1, label = label)
  list(
    label = label,
    fit = function(train, ctx) {
      gp <- games_played_tbl(train)
      out <- list()
      for (nm in c("pace", "ast", "oreb", "to")) {
        st <- prod_stats[[nm]]; w <- load_weights(ctx$league, st, ctx$season)
        cf <- cached_ridge(ctx, paste(label, nm), train, st, prod_features[[st]],
                           w$single$wgt, lambda_mult = 1 + w$single$lambda_adj)
        if (priors && !is.null(ctx$priors)) cf <- blend_with_prior(cf, ctx$priors[[nm]], gp, w$prior)
        out[[nm]] <- coef_vec(cf)
      }
      tx <- train %>% filter(!is.na(x_rtg)) %>% mutate(soe = rtg - x_rtg)
      txr <- if (impute_missing) mutate(train, x_rtg = ifelse(is.na(x_rtg), rtg, x_rtg)) else tx
      w <- load_weights(ctx$league, "rtg", ctx$season)
      xr <- cached_ridge(ctx, paste(label, "xrtg"), txr, "x_rtg", prod_features$rtg,
                         w$single$wgt, lambda_mult = 1 + w$single$lambda_adj)
      if (priors && !is.null(ctx$priors)) {
        pr <- ctx$priors$rtg %>% filter(grepl("^(team_id_|opp_id_)", name))
        xb <- blend_with_prior(xr %>% filter(grepl("^(team_id_|opp_id_)", name)), pr, gp, w$prior)
        xr <- bind_rows(xr %>% filter(!grepl("^(team_id_|opp_id_)", name)), xb)
      }
      soe_feats <- c("game_date", "team_id", "opp_id", if (soe_situational) sit)
      so <- cached_ridge(ctx, paste(label, "soe"), tx, "soe", soe_feats, soe_wgt,
                         lambda_mult = soe_lambda_mult)
      out$rtg <- coef_vec(xr)
      out$soe <- coef_vec(so)
      out
    },
    prior_state = function(pr) {
      s <- lapply(pr[c("pace", "ast", "oreb", "to", "rtg")], coef_vec)
      s$soe <- c("(Intercept)" = 0); s
    },
    predict = function(state, g) {
      p <- comp$predict(state[c("pace", "ast", "oreb", "to", "rtg")], g)
      sv <- state$soe
      z <- function(n) ifelse(is.na(sv[n]), 0, sv[n])
      soe_pt <- function(o, d, home, rest, dist) {
        neu <- as.numeric(g$neutral)
        x <- z("(Intercept)") + ifelse(is.na(lookup(sv, paste0("team_id_", o))), 0, lookup(sv, paste0("team_id_", o))) +
          ifelse(is.na(lookup(sv, paste0("opp_id_", d))), 0, lookup(sv, paste0("opp_id_", d)))
        if (soe_situational) x <- x + z("is_home") * home * (1 - neu) + z("neutral_site") * neu +
          z("days_rest") * rest + z("travel") * dist
        x
      }
      sh <- soe_pt(g$home_id, g$away_id, 1, g$home_rest, g$home_dist)
      sa <- soe_pt(g$away_id, g$home_id, 0, g$away_rest, g$away_dist)
      p$mu_home <- p$mu_home + unname(sh) * p$pace / 100
      p$mu_away <- p$mu_away + unname(sa) * p$pace / 100
      p
    }
  )
}
