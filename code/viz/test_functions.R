dynamic_potgrowth_data <- function(
    data,
    parties,
    issues,
    ses_interactions = character(0),
    model_variables = c("region", "age_cat", "income", "male", "educ", "lang", "religion")
){
  # Stop the function if more than two SES variables are provided
  if (length(ses_interactions) > 2) {
    stop("The function only supports up to two interactions with SES variables.")
  }
  model_variables <- model_variables[!(model_variables %in% ses_interactions)]
  data_model <- data %>%
    select(irc, voter, party, issue, position, all_of(ses_interactions), all_of(model_variables))
  ## for each party
  for (i in 1:length(parties)){
    partyi <- parties[i]
    data_model_party <- data_model %>%
      filter(party == partyi)
    ## for each issue
    for (j in 1:length(issues)){
      issue_j <- issues[j]
      data_model_j <- data_model_party %>%
        filter(issue == issue_j) %>% 
        mutate(position = factor(position, ordered = FALSE))
      data_model_j$position <- relevel(data_model_j$position,
                                       ref = potgrowth::mode(data_model_j$position))
      ## if no ses_interactions
      if (length(ses_interactions) == 0){
        model_irc <- data_model_j %>% 
          filter(voter == 0) %>% 
          select(irc, position, all_of(model_variables)) %>%
          lm(irc ~ .,
             data = .)
        preds_irc <- marginaleffects::predictions(
          model_irc,
          newdata = marginaleffects::datagrid(
            model = model_irc,
            position = levels(data_model_j$position)
          ),
          conf_level = 0.99
        ) %>%
          select(
            position,
            estimate_irc = estimate,
            conf_low_irc = conf.low,
            conf_high_irc = conf.high
          )
        model_vote <- data_model_j %>%
          select(voter, position, all_of(model_variables)) %>%
          glm(voter ~ .,
              data = .,
              family = binomial(link = "logit"))
        preds_vote <- marginaleffects::predictions(
          model_vote,
          newdata = marginaleffects::datagrid(
            model = model_vote,
            position = levels(data_model_j$position)
          ),
          type = "response",
          conf_level = 0.99
        ) %>%
          select(
            position,
            estimate_vote = estimate,
            conf_low_vote = conf.low,
            conf_high_vote = conf.high
          )
        df_preds_j <- left_join(preds_irc, preds_vote, by = "position") %>%
          mutate(
            issue = issue_j,
            position = as.character(position)
          ) %>% 
          relocate(issue)
        if (j == 1){
          df_preds_i <- df_preds_j
        } else {
          df_preds_i <- rbind(df_preds_i, df_preds_j)
        }
        ### only 1 ses_interactions
      } else if (length(ses_interactions) == 1){
        ## irc model
        irc_formula <- as.formula("irc ~ . + position * ses")
        model_irc <- data_model_j %>% 
          filter(voter == 0) %>% 
          rename(ses = ses_interactions[1]) %>%
          select(irc, position, ses, all_of(model_variables)) %>%
          lm(formula = irc_formula,
             data = .)
        preds_irc <- marginaleffects::predictions(
          model_irc,
          newdata = marginaleffects::datagrid(
            model = model_irc,
            position = levels(data_model_j$position),
            ses = unique(model_irc$model$ses)
          ),
          conf_level = 0.99
        ) %>%
          select(
            position,
            ses,
            estimate_irc = estimate,
            conf_low_irc = conf.low,
            conf_high_irc = conf.high
          )
        ## vote model
        vote_formula <- as.formula("voter ~ . + position * ses")
        model_vote <- data_model_j %>%
          rename(ses = ses_interactions[1]) %>%
          select(voter, position, ses, all_of(model_variables)) %>%
          glm(formula = vote_formula,
              data = .,
              family = binomial(link = "logit"))
        preds_vote <- marginaleffects::predictions(
          model_vote,
          newdata = marginaleffects::datagrid(
            model = model_vote,
            position = levels(data_model_j$position),
            ses = unique(model_vote$model$ses)
          ),
          type = "response",
          conf_level = 0.99
        ) %>%
          select(
            position,
            ses,
            estimate_vote = estimate,
            conf_low_vote = conf.low,
            conf_high_vote = conf.high
          )
        df_preds_j <- left_join(preds_irc, preds_vote, by = c("position", "ses")) %>%
          mutate(
            issue = issue_j,
            position = as.character(position)
          ) %>% 
          rename(!!ses_interactions[1] := ses) %>%
          relocate(issue)
        if (j == 1){
          df_preds_i <- df_preds_j
        } else {
          df_preds_i <- rbind(df_preds_i, df_preds_j)
        }
        ## if 2 ses_interactions
      } else if (length(ses_interactions == 2)){
        ## irc model
        irc_formula <- as.formula("irc ~ . + position * ses1 * ses2")
        model_irc <- data_model_j %>% 
          filter(voter == 0) %>% 
          rename(ses1 = ses_interactions[1],
                 ses2 = ses_interactions[2]) %>%
          select(irc, position, ses1, ses2, all_of(model_variables)) %>%
          lm(formula = irc_formula,
             data = .)
        preds_irc <- marginaleffects::predictions(
          model_irc,
          newdata = marginaleffects::datagrid(
            model = model_irc,
            position = levels(data_model_j$position),
            ses1 = unique(model_irc$model$ses1),
            ses2 = unique(model_irc$model$ses2)
          ),
          conf_level = 0.99
        ) %>%
          select(
            position,
            ses1,
            ses2,
            estimate_irc = estimate,
            conf_low_irc = conf.low,
            conf_high_irc = conf.high
          )
        ## vote model
        vote_formula <- as.formula("voter ~ . + position * ses1 * ses2")
        model_vote <- data_model_j %>%
          rename(ses1 = ses_interactions[1],
                 ses2 = ses_interactions[2]) %>%
          select(voter, position, ses1, ses2, all_of(model_variables)) %>%
          glm(formula = vote_formula,
              data = .,
              family = binomial(link = "logit"))
        preds_vote <- marginaleffects::predictions(
          model_vote,
          newdata = marginaleffects::datagrid(
            model = model_vote,
            position = levels(data_model_j$position),
            ses1 = unique(model_vote$model$ses1),
            ses2 = unique(model_vote$model$ses2)
          ),
          type = "response",
          conf_level = 0.99
        ) %>%
          select(
            position,
            ses1,
            ses2,
            estimate_vote = estimate,
            conf_low_vote = conf.low,
            conf_high_vote = conf.high
          )
        df_preds_j <- left_join(preds_irc, preds_vote, by = c("position", "ses1", "ses2")) %>%
          mutate(
            issue = issue_j,
            position = as.character(position)
          ) %>% 
          rename(!!ses_interactions[1] := ses1,
                 !!ses_interactions[2] := ses2) %>%
          relocate(issue)
        if (j == 1){
          df_preds_i <- df_preds_j
        } else {
          df_preds_i <- rbind(df_preds_i, df_preds_j)
        }
      }
    }
    ## end of loop j
    df_preds_i <- df_preds_i %>%
      mutate(party = partyi) %>% 
      relocate(party)
    if (i == 1){
      df_preds <- df_preds_i
    } else {
      df_preds <- rbind(df_preds, df_preds_i)
    }
    message(partyi)
  }
  return(df_preds)
}

test <- dynamic_potgrowth_data(
  data = data,
  parties = c("CAQ", "QS"),
  issues = issues,
  ses_interactions = "region",
)
