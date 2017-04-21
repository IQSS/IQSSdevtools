#' Add Disqus commenting integreation to a package's website landing page
#'
#' @param pkg package description, can be path or package name. See
#'   \code{\link{as.package}} for more information
#' @param shortname character string with the website's Disqus short name
#' @param page_url character string with the website's URL
#' @param page_identifier character string with the website's Disqus page
#'   identifier
#'
#' @details Before using, visit
#'   <https://disqus.com/profile/login/?next=/admin/create/> to register
#'   your website with Disqus.
#'   `use_disqus` assumes that your package documentation landing page
#'   is generated from a file called index.Rmd, e.g. as created by
#'   init_iqss_package or pkgdown.
#'
#' @importFrom whisker whisker.render
#'
#' @export

use_disqus <- function(pkg = '.', shortname, page_url, page_identifier) {

    original_path <- getwd()
    pkg <- as.package(pkg)
    path <- pkg$path
    setwd(path)

    files <- list.files()
    if ('index.Rmd' %in% files)
        index <- 'index.Rmd'
    else
        stop('Unable to find index.Rmd in %s', pkg$package)

    template <- "<div id=\"disqus_thread\"></div>
<script>
    var disqus_config = function () {
        {{page_url}} = PAGE_URL;
        {{page_identifier}} = PAGE_IDENTIFIER;
    };
    */
    (function() {
    var d = document, s = d.createElement('script');

    s.src = '//{{shortname}}.disqus.com/embed.js';

    s.setAttribute('data-timestamp', +new Date());
    (d.head || d.body).appendChild(s);
    })();
    </script>
    <noscript>Please enable JavaScript to view the <a href=\"https://disqus.com/?ref_noscript\" rel=\"nofollow\">comments powered by Disqus.</a></noscript>"

    data_list <- list(shortname = shortname,
                      page_url = page_url,
                      page_identifier = page_identifier)

    disqus_js <- whisker.render(template, data_list)

    cat(disqus_js, file = index, append = TRUE)
}
