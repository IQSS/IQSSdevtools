## EXPERIMENTAL ##
# checks links asynchronously
# not currently fully functioning
check_links <- function(links) {
    pool <- new_pool()
    good_links_list <- list()
    success <- function(req) {good_links_list <<- c(good_links_list, list(req))}
    for (link in links) {
        curl_fetch_multi(link, done = success, pool = pool)
    }
    multi_run(pool = pool)
    good_links <- ""
    for (result in good_links_list) {
        cat(result$url, result$status, "\n")
        if (result$status == 200) {
            good_links <- c(good_links, result$url)
        }
    }
    if (length(good_links)==1) return(links)
    good_links<-good_links[2:length(good_links)]
    return(setdiff(links,good_links))
}
