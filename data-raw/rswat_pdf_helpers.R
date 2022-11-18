#' Read the SWAT+ inputs documentation PDF
#'
#' Code to scrape the SWAT+ inputs documentation PDF for variable names and definitions.
#'
#' These functions are not exported by the rwsat package - you won't see them when you load
#' the package. They are called by rswat_docs.R, which should be found in /data-raw (along
#' with this file)
#'
#' So far this is tested on the 'inputs_swatplus_rev60_5.pdf' file only, which is current
#' as of October, 2022. Call `rswat_open_io(pdf_path)` (where pdf_path points to the file)
#' to make the data frame. `pdftools::pdf_text` is used to render the PDF as plain text,
#' and `rswat_parse_io_pdf` does most of the parsing.
#'
#' The regular expressions below control how variable definitions are found and grouped. They
#' identify different elements of documentation text based on their formatting. This will
#' probably require tweaking when the documentation PDF is next updated.

# header/footer (appears on most pages) and variable name table headers
regex_header = paste0('(', paste(c('^(\\s*[0-9]+\\s*SWAT\\+)',
                                   '^(\\s*SWAT\\+ INPUTS\\s*[0-9]+)',
                                   '[Vv]ariable\\s+name\\s+[Dd]efinition'), collapse=')|('), ')')

# section headings begin with an all-caps keyword, then a space, then a dash
regex_section_title = '^[A-Z]+ - .+'

# file name headings are all caps, with a 2-3 character extension
regex_file_name_main = '^[A-Z]+[-_]*[A-Z]*[-_]*[A-Z]+\\.[A-Z]{2,3}$'
regex_file_name_alt = '^recall\\_day\\.rec$'
regex_file_name = paste0('(', regex_file_name_main, ')|(', regex_file_name_alt, ')')

# variables definitions always start with the name in all-caps snake case
regex_vname_def_main = '^\\s*[A-Z][A-Z0-9]*_*[A-Z0-9]*_*[A-Z0-9]*'

# sometimes there is also a suffix attached to the variable name
regex_vname_def_suffix = paste0('(', paste(c('\\(MON\\)',
                                             ', cont\\.',
                                             '\\(layer \\#\\)',
                                             '\\(top\\slayer\\)',
                                             '[0-9]\\-[0-9]+'), collapse=')|('), ')')

# variable names to exclude ("N" is a false positive, and the others are uninformative)
regex_vname_exclude = '(^N$)|(^ID$)|(^TITLE$)|(^HEADER$)'

# after 2+ white-space characters, the mixed case definition text begins
regex_vname_def_desc = '(\\s{2,})([A-z].+[a-z]{2,})|($)'
regex_vname_def = paste0('(', regex_vname_def_main, ')',
                         '(', regex_vname_def_suffix, ')?',
                         '(', regex_vname_def_desc, ')')


#' Scrape variable definitions from the SWAT+ inputs documentation PDF
#'
#' Makes the data frame by calling `rswat_parse_io_pdf` and collapsing the results
#'
#' @param pdf_path character, path to the pdf
#' @return a data frame
rswat_open_io = function(pdf_path)
{
  # open and parse the pdf
  pdf_df = rswat_parse_io_pdf(pdf_path)

  # omit intro text
  is_intro = is.na(pdf_df[['file']])
  all_files = stats::setNames(nm=unique(pdf_df[['file']][!is_intro]))

  # split by file name and process contents in a loop
  f_df_list = lapply(all_files, \(f) {

    # copy the subset of the data frame and identify variable declarations
    f_df = subset(pdf_df, file==f)
    is_vname = f_df[['is_vname']]

    # erase variable names from their declaration lines (leaving description only)
    f_df[['type']][is_vname] = 'description'
    f_df[['string']][is_vname] = f_df[is_vname, c('string', 'vname')] |>
      apply(1L, \(x) trimws(sub(toupper(x[2]), '', x[1])))

    # extract descriptions by variable
    vname_check = unique(f_df[['vname']])
    desc_list = vname_check |>
      lapply(\(v) subset(f_df, (vname==v) & (type=='description') )[['string']])

    # combine everything so far into a data frame
    idx_out = match(vname_check, f_df[['vname']])
    out_df = data.frame(page_num = f_df[['page_num']][idx_out],
                        line_num = f_df[['line_num']][idx_out],
                        file = f,
                        name = vname_check)

    # add descriptions by collapsing components from all lines
    out_df[['desc']] = sapply(desc_list, \(s) trimws(paste(trimws(s), collapse=' ')))
    out_df[['desc_long']] = sapply(desc_list, \(s) paste(s, collapse='\n'))

    # omit empty definitions and redundant "title" definition
    out_df = subset(out_df, !is.na(name) & (nchar(desc) > 0))
    out_df = subset(out_df, name != 'title')

    # cobble any remaining text together into a comment string for the file
    comment_string = paste(subset(f_df, type=='unknown')[['string']], collapse='\n')
    attr(out_df, 'comment') = comment_string
    return(out_df)
  })

  # collect all comment strings
  comment = sapply(f_df_list, \(x) trimws(attr(x, 'comment')))

  # combine file-wise results into big data frame and reset row names
  out_df = do.call(rbind, f_df_list)
  rownames(out_df) = NULL

  return(list(defs=out_df, comment=comment))
}

#' Classify the lines in the SWAT+ inputs documentation PDF
#'
#' Loads the PDF and labels each line of the PDF as a section header, a file name
#' definition, a variable name definition, or a component of description text.
#'
#' @param pdf_path character, path to the pdf
#' @return a data frame
#'
rswat_parse_io_pdf = function(pdf_path)
{
  # open the pdf as list of plain text strings, then vectorize
  pdf_txt_stor = pdftools::pdf_text(pdf_path)
  pdf_delim = strsplit(pdf_txt_stor, '\n')
  pdf_delim_txt = unlist(pdf_delim)

  # count lines per page
  n_line_list = sapply(pdf_delim, length)
  is_empty = n_line_list == 0

  # omit pages without any text content
  n_line_list = n_line_list[!is_empty]
  page_num_list = seq_along(pdf_delim)[!is_empty]

  # initialize a data frame of line-by-line information
  pdf_df = data.frame(

    is_header = grepl(regex_header, pdf_delim_txt),
    is_section = grepl(regex_section_title, pdf_delim_txt),
    is_fname = grepl(regex_file_name, pdf_delim_txt),
    is_vname = grepl(regex_vname_def, pdf_delim_txt),
    string = pdf_delim_txt,
    string_short = substr(pdf_delim_txt, 1, 50),
    page_num = do.call(c, Map(\(p, n) rep(p, n), p=page_num_list, n=n_line_list)),
    line_num = do.call(c, lapply(n_line_list, \(n) seq(n))),
    is_overview = FALSE,
    file = NA,
    type = NA,
    vname = NA
  )

  # forward-fill the 'file' attribute with 'string' of the previous header row
  idx_fname = which(pdf_df[['is_fname']])
  n_rep = diff(c(idx_fname, 1L + nrow(pdf_df)))
  idx_string = c(rep(NA, idx_fname[1]-1L), do.call(c, Map(\(i, n) rep(i, n), n=n_rep, i=idx_fname)))
  pdf_df[['file']] = pdf_df[['string']][ idx_string ]

  # classify all lines as either as belonging to one of several classes of text
  pdf_df[['type']][ !is.na(pdf_df[['file']]) & pdf_df[['is_vname']] ] = 'variable_name'
  pdf_df[['type']][ idx_fname ] = 'file_name'
  pdf_df[['type']][ is.na(pdf_df[['type']]) ] = 'unknown'
  pdf_df[['type']][ pdf_df[['is_section']] ] = 'section'
  pdf_df[['type']][ pdf_df[['is_header']] ] = 'header'

  # extract variable names
  is_vname = pdf_df[['is_vname']]
  vname_string = pdf_df[['string']][ is_vname ]
  pdf_df[['vname']][is_vname] = trimws(regmatches(vname_string,
                                                  regexpr(regex_vname_def_main, vname_string)))

  # remove some excluded words from variable names list
  is_excluded = grepl(regex_vname_exclude, pdf_df[['vname']])
  pdf_df[['is_vname']][is_excluded] = FALSE
  pdf_df[['is_header']][is_excluded] = TRUE
  pdf_df[['type']][is_excluded] = 'header'

  # forward-fill the 'vname' attribute with value of previous definition row
  idx_vname = which(pdf_df[['is_vname']])
  n_rep = diff(c(idx_vname, 1L + nrow(pdf_df)))
  idx_desc = c(rep(NA, idx_vname[1]-1L), do.call(c, Map(\(i, n) rep(i, n), n=n_rep, i=idx_vname)))
  pdf_df[['vname']] = pdf_df[['vname']][ idx_desc ]
  pdf_df[['is_vname']] = pdf_df[['is_vname']] & !pdf_df[['is_header']]

  # classify variable description lines
  is_desc = rowSums(pdf_df[c('is_header', 'is_fname', 'is_vname')]) == 0
  pdf_df[['type']][ is_desc ] = 'description'

  # mark as unknown any text lying between fname and its first vname
  idx_vname = which(pdf_df[['is_vname']])
  idx_unknown_list = lapply(idx_fname, \(i) seq(i + 1L, idx_vname[which(idx_vname > i)[1]] - 1L))
  idx_unknown = unlist(idx_unknown_list)
  pdf_df[['vname']][idx_unknown] = NA
  pdf_df[['type']][idx_unknown] = 'unknown'

  # fix header tags that were overwritten by previous line
  pdf_df[['type']][ pdf_df[['is_header']] ] = 'header'

  # switch to lower case names and remove unwanted suffixes
  pdf_df |>
    dplyr::mutate( file = tolower(file) ) |>
    dplyr::mutate( vname = tolower(vname) ) |>
    dplyr::mutate( vname = gsub(regex_vname_def_suffix, '', vname, perl=TRUE) )
}

#' Debugging tool for `rswat_parse_io_pdf`
#'
#' This appends the classification assigned by `rswat_parse_io_pdf` (as a text string
#' prefix) to each line of the PDF text, and prints the result to the console.
#'
#' @param pdf_df the data frame returned by `rswat_parse_io_pdf`
#' @param page_num the first page number to display
#' @param len the number of pages to display
#' @return nothing
rswat_debug_io_pdf = function(pdf_df, page_num=1, len=1)
{
  text_prefix_symbols = c(unknown = '!......?......>',
                          file_name = '!====fname---->',
                          variable_name = '!=====vdef---->',
                          description = '     L-------->',
                          header = '!---header---->')

  # appends the classification to each line (pipe to cat to view)
  pdf_df[['prefix']] = text_prefix_symbols[match(pdf_df[['type']], names(text_prefix_symbols))]
  pdf_df[['_string']] = do.call(paste, pdf_df[c('prefix', 'string')])

  # number of pages in the document
  n = max(pdf_df[['page_num']])

  # convert start page to value
  if( n < page_num ) stop(paste('page_num cannot exceed', n))

  # find the sequence of lines to print
  len = pmin(len, n - page_num + 1L)
  page_out = seq(page_num, page_num + len)
  line_out = which(pdf_df[['page_num']] %in% page_out)

  # print to console
  cat(paste(pdf_df[['_string']][line_out], collapse='\n'))
  return(invisible())
}
