## Quick wrapper to read in tweet data with vroom with correct data formats ##


vroom_twitter <- function(data, # file to read in
                          list_cols =   c( # columns to unflatten and turn back into list cols
                            "hashtags",
                            "ext_urls",
                            "ext_urls_expanded",
                            "ext_urls_unwound",
                            "ext_urls_title",
                            "ext_urls_description",
                            "context.domain.id",
                            "context.domain.name",
                            "context.domain.description",
                            "context.entity.id",
                            "context.entity.name",
                            "context.entity.description",
                            "mentions_username",
                            "mentions_user_id",
                            "annotation_probability",
                            "annotation_type",
                            "annotation_entity"
                          ), 
                          col_types = list(   # column type specification
                            tweet_id = "c",
                            conversation_id = "c",
                            author_id = "c",
                            in_reply_to_user_id = "c",
                            user_pinned_tweet_id = "c",
                            sourcetweet_id = "c",
                            sourcetweet_author_id = "c",
                            hashtags = "c"
                          )) {
  require(vroom)
  vroom(data, col_types = col_types) %>%
    mutate(across(.cols = any_of(list_cols), ~ ifelse(
      is.na(.x), NA, str_split(.x, pattern = ", ") %>% as.list()
    )))
}
