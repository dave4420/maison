# maison

A custom webserver for my (dave4420's) personal use at home.

Includes a library intended to make it difficult to make responses that are
non-compliant with the HTTP specification, while making it easy to do the
right thing w.r.t. caching etc.   Said library is incomplete, hence why I
have not uploaded it to Hackage.   Prod me if you think I should upload it,
I'll change the license and add documentation.

The webserver executable serves files from homes directories.

 *  `*.markdown` files are processed by pandoc
 *  `*.journal` files are processed by hledger
 *  plain text files may be formatted as a pdf (usually 2-up by default)
    for printing (requires `a2ps` and `ps2pdf`)

