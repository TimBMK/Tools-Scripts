#' Bind information stored as JSON files
#'
#' This function binds information stored as JSON files. The experimental function `convert_json` converts individual JSON files into either "raw" or "tidy" format. 
#' 
#' By default, `bind_tweets` binds into a data frame containing tweets (from data_*id*.json files). 
#' 
#' If users is TRUE, it binds into a data frame containing user information (from users_*id*.json). 
#'
#' @param data_path string, file path to directory of stored tweets data saved as data_*id*.json and users_*id*.json
#' @param user If `FALSE`, this function binds JSON files into a data frame containing tweets; data frame containing user information otherwise. Ignore if `output_format` is not NA
#' @param verbose If `FALSE`, messages are suppressed
#' @param output_format
#' `r lifecycle::badge("experimental")` string, if it is not NA, this function return an unprocessed data.frame containing either tweets or user information. Currently, this function supports the following format(s)
#' \itemize{
#'    \item{"raw"}{List of data frames; Note: not all data frames are in Boyce-Codd 3rd Normal Form}
#'    \item{"tidy"}{Tidy format; all essential columns are available}
#'    \item{"tidy2"}{Tidy format; additional variables (see vars) are available. Untruncates retweet text and adds indicators for retweets, quotes and replies. Automatically drops duplicated tweets. Handling of quoted tweets can be specified (see quoted_variables)}
#' }
#' #' @param vars
#' `r lifecycle::badge("experimental")` vector of strings, determining additional variables provided by the tidy2 format. Can be any (or all) of the following:
#' \itemize{
#'    \item{"hashtags"}{Hashtags contained in the tweet. Untrunctated for retweets}
#'    \item{"ext_urls"}{Shortened and expanded URLs contained in the tweet, excluding those internal to Twitter (e.g. retweet URLs). Includes additional data provided by Twitter, such as the unwound URL, their title and description (if available). Untrunctated for retweets}
#'    \item{"mentions"}{Mentioned usernames and their IDs, excluding retweeted and quoted users. Untrunctated for retweets}
#'    \item{"annotations"}{Annotations provided by Twitter, including their probability and type. Basically Named Entities. See }
#'    \item{"context_annotations"}{Context annotations provided by Twitter, including additional data on their domains. See }
#' }
#' 
#' #' @param quoted_variables
#' `r lifecycle::badge("experimental")` Defines handling of entities for quotes. When TRUE, additional variables (see vars) are taken from the quoted tweet. When FALSE, variables are taken only from the tweet text added by the author. Defaults to FALSE
#' 
#' @return a data.frame containing either tweets or user information
#' @export
#'
#' @examples
#' \dontrun{
#' # bind json files in the directory "data" into a data frame containing tweets
#' bind_tweets(data_path = "data/")
#' 
#' # bind json files in the directory "data" into a data frame containing user information
#' bind_tweets(data_path = "data/", user = TRUE)
#' 
#' # bind json files in the directory "data" into a "tidy" data frame / tibble
#' bind_tweets(data_path = "data/", user = TRUE, output_format = "tidy")
#' 
#' # bind json files in the directory "data" into a "tidy" data frame / tibble with additional variables
#' bind_tweets(data_path = "data/", user = TRUE, output_format = "tidy", vars = c("hashtags", "ext_urls", "mentions", "annotations", "context_annotations"))
#' }
bind_tweets <- function(data_path, user = FALSE, verbose = TRUE, output_format = NA, 
                        vars = c("hashtags", "ext_urls", "mentions", "annotations", "context_annotations"),
                        quoted_variables = F) {
  if (!is.na(output_format)) {
    return(.flat(data_path, output_format = output_format, vars = vars, quoted_variables = quoted_variables))
  }
  if(user) {
    files <- ls_files(data_path, "^users_")
  } else {
    files <- ls_files(data_path, "^data_")
  }
  if (verbose) {
    pb <- utils::txtProgressBar(min = 0, max = length(files), initial = 0)
  }  
  json.df.all <- data.frame()
  for (i in seq_along(files)) {
    filename <- files[[i]]
    json.df <- jsonlite::read_json(filename, simplifyVector = TRUE)
    if (user) {
      json.df <- json.df$users
    }
    json.df.all <- dplyr::bind_rows(json.df.all, json.df)
    if (verbose) {
      utils::setTxtProgressBar(pb, i)
    }
  }
  .vcat(verbose, "\n")
  return(json.df.all)
}

ls_files <- function(data_path, pattern) {
  ## parse and bind
  files <-
    list.files(
      path = file.path(data_path),
      pattern = pattern,
      recursive = TRUE,
      include.dirs = TRUE,
      full.names = TRUE
    )
  
  if (length(files) < 1) {
    stop(paste0("There are no files matching the pattern `", pattern, "` in the specified directory."), call. = FALSE)
  }
  return(files)
}

#' @param data_file string, a single file path to a JSON file; or a vector of file paths to JSON files of stored tweets data saved as data_*id*.json
#' @export
#' @rdname bind_tweets
#' @importFrom rlang .data
convert_json <- function(data_file, output_format = "tidy",
                         vars = c("hashtags", "ext_urls", "mentions", "annotations", "context_annotations"),
                         quoted_variables = F) {
  if (!output_format %in% c("tidy", "raw", "tidy2")) {
    stop("Unknown format.", call. = FALSE)
  }
  tweet_data <- .gen_raw(purrr::map_dfr(data_file, ~jsonlite::read_json(., simplifyVector = TRUE)))
  names(tweet_data) <- paste0("tweet.", names(tweet_data))
  aux_file <- .gen_aux_filename(data_file)
  user_data <- .gen_raw(purrr::map_dfr(aux_file, ~jsonlite::read_json(., simplifyVector = TRUE)$users), pki_name = "author_id")
  names(user_data) <- paste0("user.", names(user_data))
  sourcetweet_data <- list(main = purrr::map_dfr(aux_file, ~jsonlite::read_json(., simplifyVector = TRUE)$tweets))
  names(sourcetweet_data) <- paste0("sourcetweet.", names(sourcetweet_data))
  ## raw
  raw <- c(tweet_data, user_data, sourcetweet_data)
  if (output_format == "raw") {
    return(raw)
  }
  if (output_format == "tidy") {
    tweetmain <- raw[["tweet.main"]]
    usermain <- dplyr::distinct(raw[["user.main"]], .data$author_id, .keep_all = TRUE)  ## there are duplicates
    colnames(usermain) <- paste0("user_", colnames(usermain))
    tweet_metrics <- tibble::tibble(tweet_id = raw$tweet.public_metrics.retweet_count$tweet_id,
                                    retweet_count = raw$tweet.public_metrics.retweet_count$data,
                                    like_count = raw$tweet.public_metrics.like_count$data,
                                    quote_count = raw$tweet.public_metrics.quote_count$data)
    user_metrics <- tibble::tibble(author_id = raw$user.public_metrics.tweet_count$author_id,
                                   user_tweet_count = raw$user.public_metrics.tweet_count$data,
                                   user_list_count = raw$user.public_metrics.listed_count$data,
                                   user_followers_count = raw$user.public_metrics.followers_count$data,
                                   user_following_count = raw$user.public_metrics.following_count$data) %>%
      dplyr::distinct(.data$author_id, .keep_all = TRUE)
    res <- tweetmain %>% dplyr::left_join(usermain, by = c("author_id" = "user_author_id")) %>%
      dplyr::left_join(tweet_metrics, by = "tweet_id") %>%
      dplyr::left_join(user_metrics, by = "author_id")
    if (!is.null(raw$tweet.referenced_tweets)) {
      ref <- raw$tweet.referenced_tweets
      colnames(ref) <- c("tweet_id", "sourcetweet_type", "sourcetweet_id")
      ref <- ref %>% dplyr::filter(.data$sourcetweet_type != "replied_to")
      res <- dplyr::left_join(res, ref, by = "tweet_id")
      if (nrow(raw$sourcetweet.main) > 0) {
        source_main <- dplyr::select(raw$sourcetweet.main, .data$id, .data$text, .data$lang, .data$author_id) %>%
          dplyr::distinct(.data$id, .keep_all = TRUE)
        colnames(source_main) <- paste0("sourcetweet_", colnames(source_main))
        res <- res %>% dplyr::left_join(source_main, by = "sourcetweet_id")
      }
    }
    res <- dplyr::relocate(res, .data$tweet_id, .data$user_username, .data$text)
    return(tibble::as_tibble(res))
  }

  if (output_format == "tidy2") {
    tweetmain <- raw[["tweet.main"]]
    usermain <- dplyr::distinct(raw[["user.main"]], .data$author_id, .keep_all = TRUE)  ## there are duplicates
    colnames(usermain) <- paste0("user_", colnames(usermain))
    tweet_metrics <- tibble::tibble(tweet_id = raw$tweet.public_metrics.retweet_count$tweet_id,
                                    retweet_count = raw$tweet.public_metrics.retweet_count$data,
                                    like_count = raw$tweet.public_metrics.like_count$data,
                                    quote_count = raw$tweet.public_metrics.quote_count$data)
    user_metrics <- tibble::tibble(author_id = raw$user.public_metrics.tweet_count$author_id,
                                   user_tweet_count = raw$user.public_metrics.tweet_count$data,
                                   user_list_count = raw$user.public_metrics.listed_count$data,
                                   user_followers_count = raw$user.public_metrics.followers_count$data,
                                   user_following_count = raw$user.public_metrics.following_count$data) %>%
      dplyr::distinct(.data$author_id, .keep_all = TRUE)
    res <- tweetmain %>% dplyr::left_join(usermain, by = c("author_id" = "user_author_id")) %>%
      dplyr::left_join(tweet_metrics, by = "tweet_id") %>%
      dplyr::left_join(user_metrics, by = "author_id")
    if ("hashtags" %in% vars & !is.null(raw$tweet.entities.hashtags)) {
      hashtags <- raw$tweet.entities.hashtags %>% 
        dplyr::group_by(tweet_id) %>% dplyr::mutate(hashtags = if ("tag" %in% colnames(.)) list(tag) else NA) %>% # used hashtags as list per tweet
        dplyr::ungroup() %>% dplyr::distinct(tweet_id, .keep_all = TRUE) %>%    # drop duplicates created through the dplyr::left_join
        dplyr::select(tweet_id, hashtags)
      res <- dplyr::left_join(res, hashtags, by = "tweet_id") 
    }
    if ("ext_urls" %in% vars & !is.null(raw$tweet.entities.urls)) {
      urls <- raw$tweet.entities.urls  # has a row for every URL
      if ("expanded_url" %in% colnames(urls)) { # if applicable, drop twitter-intern URLs (retweets etc.)
        urls <- dplyr::filter(urls, !stringr::str_detect(expanded_url, pattern = "https://twitter.com/"))}  
      urls <- urls %>% 
        dplyr::group_by(tweet_id) %>% 
        dplyr::mutate(ext_urls = if ("url" %in% colnames(.)) list(url) else NA, # used URLs as list per tweet
                      ext_urls_expanded = if ("expanded_url" %in% colnames(.)) list(expanded_url) else NA,
                      ext_urls_unwound = if ("unwound_url" %in% colnames(.)) list(unwound_url) else NA,
                      ext_urls_title = if ("title" %in% colnames(.)) list(title) else NA,
                      ext_urls_description = if ("description" %in% colnames(.)) list(description) else NA) %>% 
        dplyr::ungroup() %>% dplyr::distinct(tweet_id, .keep_all = TRUE) %>% # drop duplicates created through the dplyr::left_join
        dplyr::select(tweet_id, tidyselect::starts_with("ext_urls"))
      res <- dplyr::left_join(res, urls, by = "tweet_id") 
    }
    if ("annotations" %in% vars & !is.null(raw$tweet.entities.annotations)) {
      annotations <- raw$tweet.entities.annotations %>% # has a row for every hashtag
        dplyr::group_by(tweet_id) %>% dplyr::mutate( # used annotations as list per tweet
          annotation_probability = if ("probability" %in% colnames(.)) list(as.numeric(probability)) else NA, 
          annotation_type = if ("type" %in% colnames(.)) list(type) else NA,
          annotation_entity = if ("normalized_text" %in% colnames(.)) list(normalized_text) else NA) %>% 
        dplyr::ungroup() %>% dplyr::distinct(tweet_id, .keep_all = TRUE) %>%  # drop duplicates created through the dplyr::left_join
        dplyr::select(tweet_id, tidyselect::starts_with("annotation"))
      res <- dplyr::left_join(res, annotations, by = "tweet_id") 
    }
    if ("context_annotations" %in% vars & !is.null(raw$tweet.context_annotations)) {
      context_annotations <- raw$tweet.context_annotations %>% 
        dplyr::group_by(tweet_id) %>% dplyr::mutate( # used annotations as list per tweet
          context.domain.id = if ("domain.id" %in% colnames(.)) list(domain.id) else NA,
          context.domain.name = if ("domain.name" %in% colnames(.)) list(domain.name) else NA,
          context.domain.description = if ("domain.description" %in% colnames(.)) list(domain.description) else NA,
          context.entity.id = if ("entity.id" %in% colnames(.)) list(entity.id) else NA,
          context.entity.name = if ("entity.name" %in% colnames(.)) list(entity.name) else NA,
          context.entity.description =  if("entity.description" %in% colnames(.)) list(entity.description) else NA) %>% 
        dplyr::ungroup() %>% dplyr::distinct(tweet_id, .keep_all = TRUE) %>%  # drop duplicates created through the dplyr::left_join
        dplyr::select(tweet_id, tidyselect::starts_with("context"))
      res <- dplyr::left_join(res, context_annotations, by = "tweet_id")
    }
    if ("mentions" %in% vars & is.null(raw$tweet.referenced_tweets)) { # failsafe for mentions if no referenced tweet data is available
      mentions <- raw$tweet.entities.mentions %>% #  a row for every mention
        dplyr::group_by(tweet_id) %>%
        dplyr::mutate(mentions_username = if ("username" %in% colnames(.)) list(as.character(username)) else NA,# mentioned usernames as list per tweet
                      mentions_user_id = if ("id" %in% colnames(.)) list(as.character(id)) else NA) %>% # mentioned users' IDs as list per tweet
        dplyr::ungroup() %>% dplyr::distinct(tweet_id, .keep_all = TRUE) %>%  # drop duplicates created through the dplyr::left_join
        dplyr::select(tweet_id, tidyselect::starts_with("mentions"))
      res <- dplyr::left_join(res, mentions, by = "tweet_id")
    }
    if (!is.null(raw$tweet.referenced_tweets)) {
      ref <- raw$tweet.referenced_tweets
      colnames(ref) <- c("tweet_id", "sourcetweet_type", "sourcetweet_id")
      ref <- ref %>% dplyr::filter(.data$sourcetweet_type != "replied_to") 
      res <- dplyr::left_join(res, ref, by = "tweet_id")
      if (nrow(raw$sourcetweet.main) > 0) {
        source_main <- dplyr::select(raw$sourcetweet.main, .data$id, .data$text, .data$lang, .data$author_id) %>%
          dplyr::distinct(.data$id, .keep_all = TRUE)
        colnames(source_main) <- paste0("sourcetweet_", colnames(source_main))
        res <- res %>% dplyr::left_join(source_main, by = "sourcetweet_id") %>% 
          dplyr::mutate(text = dplyr::case_when(  # full length text for RTs, removes RT marker
            sourcetweet_type == "retweeted" ~ sourcetweet_text,
            is.na(sourcetweet_type) ~ text,
            TRUE ~ text
          )) %>%
          dplyr::mutate(is_retweet = dplyr::case_when( # clear retweet identifier
            sourcetweet_type == "retweeted" ~ TRUE,
            is.na(sourcetweet_type) ~ FALSE,
            TRUE ~ FALSE
          )) %>% 
          dplyr::mutate(is_quote = dplyr::case_when( # clear quote identifier
            sourcetweet_type == "quoted" ~ TRUE,
            is.na(sourcetweet_type) ~ FALSE,
            TRUE ~ FALSE
          )) %>%  
          dplyr::mutate(is_reply = if ("in_reply_to_user_id" %in% colnames(.)) dplyr::case_when( # clear reply identifier (note that replies can also be e.g. quotes, when tweets are quoted in a thread)
            !is.na(in_reply_to_user_id) ~ TRUE,
            is.na(in_reply_to_user_id) ~ FALSE,
          ) else FALSE) %>%  
          dplyr::distinct(tweet_id, .keep_all = TRUE) # make unique explicitly to prevent errors (e.g. duplicated mentions). Will be made unique anyway
        if (!is.null(vars)) {
          rt <-        # RT and quote entity information. This is incomplete in the tweet.entities for RTs due to truncation and missing for quotes
            raw$sourcetweet.main %>% 
            dplyr::filter(id %in% raw$tweet.referenced_tweets[raw$tweet.referenced_tweets$type != "replied_to", "id"]$id) %>% # get retweets & quotes only
            dplyr::distinct(id, .keep_all = TRUE) # unique tweets only (requires formatting as data.table to be efficient)
          if ("mentions" %in% colnames(rt$entities)) rt$entities.mentions <- rt$entities$mentions %>% purrr::map(as.data.frame) # this is necessary because empty data is represented as Named list(),
          if ("hashtags" %in% colnames(rt$entities)) rt$entities.hashtags <- rt$entities$hashtags %>% purrr::map(as.data.frame) #  causing tidyr::unnest() (and equivalent functions) to fail due to diferent data formats
          if ("urls" %in% colnames(rt$entities)) rt$entities.urls <- rt$entities$urls %>% purrr::map(as.data.frame)
          if ("annotations" %in% colnames(rt$entities)) rt$entities.annotations <- rt$entities$annotations %>% purrr::map(as.data.frame)
          if ("context_annotations" %in% colnames(rt)) rt$context_annotations <- rt$context_annotations %>% purrr::map(as.data.frame)
        }
        if ("mentions" %in% vars & !is.null(raw$tweet.entities.mentions)) {
          mentions_nonrt <-
            dplyr::left_join(raw$tweet.entities.mentions,
                             dplyr::select(res, tweet_id, sourcetweet_author_id, is_retweet, is_quote),    # some additional variables are needed to dplyr::filter out quoted users
                             by = "tweet_id") %>%  
            dplyr::filter(is_retweet == F) %>%  # drop retweets (will be added later)
            dplyr::filter(   # drop retweeted / quoted users from the mentions
              id != sourcetweet_author_id |
                is.na(sourcetweet_author_id) |
                is.na(id)) %>% 
            dplyr::filter(is_quote == F) %>%  # drop quotes (will be added later)
            dplyr::group_by(tweet_id) %>%
            dplyr::mutate(mentions_username = if ("username" %in% colnames(.)) list(as.character(username)) else NA,# mentioned usernames as list per tweet
                          mentions_user_id = if ("id" %in% colnames(.)) list(as.character(id)) else NA) %>% # mentioned users' IDs as list per tweet
            dplyr::ungroup() %>% dplyr::distinct(tweet_id, .keep_all = TRUE) %>%  # drop duplicates created through the dplyr::left_join
            dplyr::select(tweet_id, tidyselect::starts_with("mentions")) 
          res <- dplyr::left_join(res, mentions_nonrt, by = "tweet_id")
          if (!is.null(rt$entities.mentions)){
            mentions_rt <-
              rt %>% dplyr::select(id, entities.mentions) %>% tidyr::unnest(entities.mentions, names_sep = "_", keep_empty = T) %>%  # ! requires tidyr v1.1.4+ !
              dplyr::group_by(id) %>% 
              dplyr::mutate(mentions_username_source = if ("entities.mentions_username" %in% colnames(.)) list(as.character(entities.mentions_username)) else NA,# mentioned usernames as list per tweet
                            mentions_user_id_source = if ("entities.mentions_id" %in% colnames(.)) list(as.character(entities.mentions_id)) else NA) %>% # mentioned users' IDs as list per tweet
              dplyr::select(id, tidyselect::starts_with("mentions")) %>% # dplyr::select relevant variables
              dplyr::ungroup() %>% dplyr::distinct(id, .keep_all = TRUE) # drop duplicates introduced by tidyr::unnesting
            res <- dplyr::left_join(res, mentions_rt, by = c("sourcetweet_id" = "id")) 
            res <- .merge_rt_variables(res) # get correct entities for RTs (seperately for every dataset to account for missing variables)
            res <- .merge_quote_variables(res, quoted_variables) # get correct quote entities according to preference
            res <- res %>% dplyr::select(!tidyselect::ends_with("_source")) # drop _source variables
          }
        }
        if ("ext_urls" %in% vars & !is.null(rt$entities.urls))  {        
          urls_rt <- rt %>% dplyr::select(id, entities.urls) %>% tidyr::unnest(entities.urls, names_sep = "_", keep_empty = T)   # ! requires tidyr v1.1.4+ !
          if ("entities.urls_expanded_url" %in% colnames(urls_rt)) { # if applicable drop twitter-intern URLs (retweets etc.)
            urls_rt <- dplyr::filter(urls_rt, !stringr::str_detect(entities.urls_expanded_url, pattern = "https://twitter.com/"))
          }  
          urls_rt <- urls_rt %>%   
            dplyr::group_by(id) %>% 
            dplyr::mutate(ext_urls_source = if ("entities.urls_url" %in% colnames(.)) list(entities.urls_url) else NA, # used URLs as list per tweet
                          ext_urls_expanded_source = if ("entities.urls_expanded_url" %in% colnames(.)) list(entities.urls_expanded_url) else NA,
                          ext_urls_unwound_source = if ("entities.urls_unwound_url" %in% colnames(.)) list(entities.urls_unwound_url) else NA,
                          ext_urls_title_source = if ("entities.urls_title" %in% colnames(.)) list(entities.urls_title) else NA,
                          ext_urls_description_source = if ("entities.urls_description" %in% colnames(.)) list(entities.urls_description) else NA) %>% 
            dplyr::select(id, tidyselect::starts_with("ext_urls")) %>%  # dplyr::select relevant variables
            dplyr::ungroup() %>% dplyr::distinct(id, .keep_all = TRUE) # drop duplicates introduced by tidyr::unnesting
          res <- dplyr::left_join(res, urls_rt, by = c("sourcetweet_id" = "id"))
          res <- .merge_rt_variables(res) # get correct entities for RTs 
          res <- .merge_quote_variables(res, quoted_variables) # get correct quote entities according to preference
          res <- res %>% dplyr::select(!tidyselect::ends_with("_source")) # drop _source variables
          }
        if ("hashtags" %in% vars & !is.null(rt$entities.hashtags)) {
          hashtags_rt <-
            rt %>% dplyr::select(id, entities.hashtags) %>% tidyr::unnest(entities.hashtags, names_sep = "_", keep_empty = T) %>%  
            dplyr::group_by(id) %>% 
            dplyr::mutate(hashtags_source = if ("entities.hashtags_tag" %in% colnames(.)) list(entities.hashtags_tag) else NA) %>% # hashtags as list per tweet
            dplyr::select(id, hashtags_source) %>% # dplyr::select relevant variables
            dplyr::ungroup() %>% dplyr::distinct(id, .keep_all = TRUE) # drop duplicates introduced by tidyr::unnesting
          res <- dplyr::left_join(res, hashtags_rt, by = c("sourcetweet_id" = "id"))
          res <- .merge_rt_variables(res) # get correct entities for RTs 
          res <- .merge_quote_variables(res, quoted_variables) # get correct quote entities according to preference
          res <- res %>% dplyr::select(!tidyselect::ends_with("_source")) # drop _source variables
        }
        if ("annotations" %in% vars & !is.null(rt$entities.annotations)) {
          annotations_rt <-
            rt %>% dplyr::select(id, entities.annotations) %>% tidyr::unnest(entities.annotations, names_sep = "_", keep_empty = T) %>%  
            dplyr::group_by(id) %>% 
            dplyr::mutate(annotation_probability_source = if ("entities.annotations_probability" %in% colnames(.)) list(as.numeric(entities.annotations_probability)) else NA, # used annotations as list per tweet
                          annotation_type_source = if ("entities.annotations_type" %in% colnames(.)) list(entities.annotations_type) else NA,
                          annotation_entity_source = if ("entities.annotations_normalized_text" %in% colnames(.)) list(entities.annotations_normalized_text) else NA) %>% 
            dplyr::select(id, tidyselect::starts_with("annotation")) %>% # dplyr::select relevant variables
            dplyr::ungroup() %>% dplyr::distinct(id, .keep_all = TRUE) # drop duplicates introduced by tidyr::unnesting
          res <- dplyr::left_join(res, annotations_rt, by = c("sourcetweet_id" = "id"))
          res <- .merge_rt_variables(res) # get correct entities for RTs 
          res <- .merge_quote_variables(res, quoted_variables) # get correct quote entities according to preference
          res <- res %>% dplyr::select(!tidyselect::ends_with("_source")) # drop _source variables
        }
        if ("context_annotations" %in% vars & !is.null(rt$context_annotations)) {
          context_annotations_rt <- 
            rt %>% dplyr::select(id, context_annotations) %>% tidyr::unnest(context_annotations, names_sep = "_", keep_empty = T) %>% 
            dplyr::group_by(id) %>% dplyr::mutate( # used annotations as list per tweet
              context.domain.id_source = if ("id" %in% (.[["context_annotations_domain"]] %>% colnames(.))) list(context_annotations_domain$id) else NA, 
              context.domain.name_source = if ("name" %in% (.[["context_annotations_domain"]] %>% colnames(.))) list(context_annotations_domain$name) else NA,
              context.domain.description_source = if ("description" %in% (.[["context_annotations_domain"]] %>% colnames(.))) list(context_annotations_domain$description) else NA,
              context.entity.id_source = if ("id" %in% (.[["context_annotations_entity"]] %>% colnames(.))) list(context_annotations_entity$id) else NA,
              context.entity.name_source = if ("name" %in% (.[["context_annotations_entity"]] %>% colnames(.))) list(context_annotations_entity$name) else NA,
              context.entity.description_source = if ("description" %in% (.[["context_annotations_entity"]] %>% colnames(.))) list(context_annotations_entity$description) else NA) %>% 
            dplyr::ungroup() %>% dplyr::distinct(id, .keep_all = TRUE) %>%  # drop duplicates created through the dplyr::left_join
            dplyr::select(id, tidyselect::starts_with("context."))
          res <- dplyr::left_join(res, context_annotations_rt, by = c("sourcetweet_id" = "id"))
          res <- .merge_rt_variables(res) # get correct entities for RTs
          res <- .merge_quote_variables(res, quoted_variables) # get correct quote entities according to preference
          res <- res %>% dplyr::select(!tidyselect::ends_with("_source")) # drop _source variables
        } 
      }
    }
    res <- res %>% dplyr::mutate(dplyr::across(.cols = everything(), ~ dplyr::na_if(., "NULL"))) # set left_join's NULL values to NA for consistency
    res <- dplyr::relocate(res, .data$tweet_id, .data$user_username, .data$text)
    return(tibble::as_tibble(res))
  }
}

.flat <- function(data_path, output_format = "tidy", vars = c("hashtags", "ext_urls", "mentions", "annotations", "context_annotations"), quoted_variables = F) {
  if (!output_format %in% c("tidy", "raw", "tidy2")) {
    stop("Unknown format.", call. = FALSE)
  }
  data_files <- ls_files(data_path, "^data_")
  if (output_format == "raw") {
    return(convert_json(data_files, output_format = "raw"))
  }
  return(purrr::map_dfr(data_files, convert_json, output_format = output_format, vars = vars, quoted_variables = quoted_variables))
}

.gen_aux_filename <- function(data_filename) {
  ids <- gsub("[^0-9]+", "" , basename(data_filename))
  return(file.path(dirname(data_filename), paste0("users_", ids, ".json")))
}

.gen_raw <- function(df, pkicol = "id", pki_name = "tweet_id") {
  dplyr::select_if(df, is.list) -> df_complex_col
  dplyr::select_if(df, Negate(is.list)) %>% dplyr::rename(pki = tidyselect::all_of(pkicol)) -> main
  ## df_df_col are data.frame with $ in the column, weird things,
  ## need to be transformed into list-columns with .dfcol_to_list below
  df_complex_col %>% dplyr::select_if(is.data.frame) -> df_df_col
  ## "Normal" list-column
  df_complex_col %>% dplyr::select_if(Negate(is.data.frame)) -> df_list_col
  mother_colnames <- colnames(df_df_col)
  df_df_col_list <- dplyr::bind_cols(purrr::map2_dfc(df_df_col, mother_colnames, .dfcol_to_list), df_list_col)
  all_list <- purrr::map(df_df_col_list, .simple_unnest, pki = main$pki)
  ## after first pass above, some columns are still not in 3NF (e.g. context_annotations)
  item_names <- names(all_list)
  all_list <- purrr::map2(all_list, item_names, .second_pass)
  all_list$main <- dplyr::relocate(main, .data$pki)
  all_list <- purrr::map(all_list, .rename_pki, pki_name = pki_name)
  return(all_list)
}

.rename_pki <- function(item, pki_name = "tweet_id") {
  colnames(item)[colnames(item) == "pki"] <- pki_name
  return(item)
}

.second_pass <- function(x, item_name) {
  ## turing test for "data.frame" columns,something like context_annotations
  if (ncol(dplyr::select_if(x, is.data.frame)) != 0) {
    ca_df_col <- dplyr::select(x, -.data$pki)
    ca_mother_colnames <- colnames(ca_df_col)
    return(dplyr::bind_cols(dplyr::select(x, .data$pki), purrr::map2_dfc(ca_df_col, ca_mother_colnames, .dfcol_to_list)))
  }
  ## if (dplyr::summarise_all(x, ~any(purrr::map_lgl(., is.data.frame))) %>% dplyr::rowwise() %>% any()) {
  ##   ca_df_col <- dplyr::select(x, -pki)
  ##   ca_mother_colnames <- colnames(ca_df_col)
  ##   res <- purrr::map(ca_df_col, .simple_unnest, pki = pki)
  ##   names(res) <- paste0(item_name, ".", names(res))
  ##   return(res)
  ## }
  return(x)
}


.dfcol_to_list <- function(x_df, mother_name) {
  tibble::as_tibble(x_df) -> x_df
  x_df_names <- colnames(x_df)
  colnames(x_df) <- paste0(mother_name, ".", x_df_names)
  return(x_df)
}

.simple_unnest <- function(x, pki) {
  if (class(x) == "list" & any(purrr::map_lgl(x, is.data.frame))) {
    tibble::tibble(pki = pki, data = x) %>% dplyr::filter(purrr::map_lgl(.data$data, ~length(.) != 0)) %>% dplyr::group_by(.data$pki) %>% tidyr::unnest(cols = c(.data$data)) %>% dplyr::ungroup() -> res
  } else {
    res <- tibble::tibble(pki = pki, data = x)
  }
  return(res)
}

.merge_rt_variables <- function(x) {
  variables <- dplyr::select(x, tidyselect::ends_with("_source")) %>% colnames(.) %>% stringr::str_extract(".*(?=_source)")
  if (!purrr::is_empty(variables)) {
      for (i in 1:length(variables)) {
        x <- x %>% dplyr::mutate(!!as.name(variables[i]) := ifelse(is_retweet == F & (as.character(variables[i]) %in% colnames(.)),   
                                                            !!as.name(variables[i]),          
                                                            !!as.name(paste0(variables[i],"_source"))))
      }
  }  
  return(x)
}

.merge_quote_variables <- function(x, quoted_variables) {
  variables <- dplyr::select(x, tidyselect::ends_with("_source")) %>% colnames(.) %>% stringr::str_extract(".*(?=_source)")
  if (!purrr::is_empty(variables)) {
    for (i in 1:length(variables)) {
      x <- x %>% dplyr::mutate(!!as.name(variables[i]) := ifelse(is_quote == T,   
                                                                   !!as.name(variables[i]),
                                                                   !!as.name(paste0(variables[i],"_source"))))
      if (quoted_variables == T ) {
        x <- x %>% dplyr::mutate(!!as.name(paste0(variables[i], "_quoted")) := ifelse(is_quote == T,   
                                                                                     !!as.name(paste0(variables[i],"_source")),          
                                                                                     !!as.name(variables[i])))       
      }
    }
  }  
  return(x)
}




# test runs
setwd("D:/academicCloud/EPINETZ/EPINetz-Team/Data/Seedlist_Publication/")

data <- bind_tweets(data_path = "timelines", output_format = "tidy2")



# academictwitteR's native function's timing for full Seedlist_publication data
# user  system elapsed 
# 653.64    8.11  726.75 



# bind_tweets > .flat > ls_files, purrr::map_dfr(convert_json) > .gen_raw



