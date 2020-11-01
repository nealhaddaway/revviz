#' Convert dataframe to a table within an HTML page
#' 
#' @description Function converts a dataframe into an HTML table embedded within a full 
#' HTML file. The output file contains a summary showing the total number of table rows. 
#' The HTML file also includes JavaScript functions to support URL suffix searching via 
#' appending '?s=[query]' to the final URL. A JavaScript function filters the table and 
#' displays matching rows (based on an exact match search across all columns), updating 
#' the disaplyed row totals in the summary line.
#' @param data A dataframe containing columns and rows of data.
#' @return an HTML object containing the full HTML code for the webpage. The function 
#' also saves the HTML file to the working directory.
#' @example 
#' data <- read.csv(file.choose())
#' attach(data)
#' html <- dataframe2html(data);
#' @export
dataframe2html <- function(data){
  htmlleading <- '<!DOCTYPE html>\n <html>\n <head>\n \t<script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"></script>\n \t<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery-csv/0.71/jquery.csv-0.71.min.js"></script>\n \t<meta name="viewport" content="width=device-width, initial-scale=2">\n \t\n \t<style type="text/css">\n .titletext {\n \tfont-weight: 400;\n \tfont-family: sans-serif;\n \tfont-size: 20px;\n }\n \n table {\n \tfont-weight: 400;\n \tline-height: 1.5;\n \tcolor: #212529;\n \tfont-family: sans-serif;\n \tmargin: 0px;\n \tpadding: 0px;\n \tbox-sizing: border-box;\n \tmargin-bottom: 110px;\n \tbackground-color: #fff;\n \tpadding-top: 5px;\n \tfont-size: 14px;\n \tword-wrap: break-word;\n }\n \n th {\n \tposition:sticky;\n \tpadding:5px;\n \ttop:0px;\n \tbackground:#c6c09c;\n \tcolor:#fff;\n \twidth: 30px;\n }\n \n td {\n \ttext-align: left;\n \tpadding: 20px;\n }\n \n tr {\n \tborder-bottom: 1px solid #ddd;\n }\n \n tr.header, tr:hover {\n \tbackground-color: #f4f3ed;\n }\n \n </style>\n \t\n </head>\n <body onload="prefill(); myFunction(); UpdateCount()">\n \t<p class="titletext">Showing <span id="rowscount" onload="myFunction()" style="color:#938D69"></span> of <span id="totcount"></span> selected studies: <span id="selection" onload="prefill()" style="color:#938D69"></span></p>\n <input type="text" id="myInput1" onload="myFunction()" hidden>\n'
  htmltrailing <- '<script>\n// extracts text from trailing end of URL and enters it into the filter\nfunction prefill (){\n\tvar loc = location.href;\n\tvar locationField1 = document.getElementById("myInput1");\n\tvar printout = document.getElementById("selection");\n\tconst var1 = loc.split("?s=").pop();\n\tvar part1 = var1.substring(\n\t\t\tvar1.lastIndexOf("?s=") + 1, \n\t\t\tvar1.lastIndexOf("")\n\t);\n\tpart1 = part1.replace(/%20/g, " ");\n\tpart1 = part1.replace(/%2F/g, "/");\n\tif (loc.includes("?s=")) {\n\t\tlocationField1.value = part1;\n\t\tprintout.innerHTML = part1;\n\t} else {}\n}\n\n// filters rows according to input values\nfunction myFunction() {\n\tvar input, filter, table, tr, td, i;\n\tinput = document.getElementById("myInput1");\n\tfilter = input.value.toUpperCase();\n\ttable = document.getElementById("tbody_id");\n\ttr = table.getElementsByTagName("tr");\n\t\n\t// embedded code to calculate number of total rows in the table\n\tvar totcounts = document.getElementById("totcount");\n\tvar output = tr.length;\n\ttotcounts.innerHTML = output;\n\t\n\tfor (var i = 0; i < tr.length; i++) {\n\t\tvar tds = tr[i].getElementsByTagName("td");\n\t\tvar flag = false;\n\t\tfor(var j = 0; j < tds.length; j++){\n\t\t\tvar td = tds[j];\n\t\t\tif (td.innerHTML.toUpperCase().indexOf(filter) > -1) {\n\t\t\t\tflag = true;\n\t\t\t} \n\t\t}\n\t\tif(flag){\n\t\t\t\ttr[i].style.display = "";\n\t\t}\n\t\telse {\n\t\t\t\ttr[i].style.display = "none";\n\t\t}\n\t}\n}\n\n// creates a count of the number of rows shown in the filtered table\nfunction UpdateCount () {\n\tvar returnt = document.getElementById("rowscount");\n\tvar totalRows = $(\'#tbody_id tr:not([style*="display: none"])\').length;\nreturnt.innerHTML = totalRows;\n}\n</script>\n</body>\n</html>\n'
  
  part1 <- paste('<table id="table_id" class="table_class">\n\t<thead id="thead_id" class="thead_class">\n\t\t<tr>\n\t\t\t<th>',
                 paste(names(data),
                       collapse = '</th>\n\t\t\t<th>',
                       sep=''),
                 '</th>\n\t\t</tr>\n\t</thead>\n\t<tbody id="tbody_id" class="tbody_class">\n',
                 sep = '')
  x <- tidyr::unite(data, 
                    'new',
                    colnames(data),
                    sep = '</td>\n\t\t\t<td>')
  part2 <- paste('\t\t<tr>\n',
                 paste(sapply(x, 
                        function(x) paste('\t\t\t<td>', 
                                          x, 
                                          '\t\t\t</td>\n', 
                                          sep = '')),
                 collapse = '\t\t</tr>\n\t\t<tr>'),
                 '\t\t</tr>\n',
                 sep = '')
  
  part3 <- paste('\t</tbody>\n</table>\n',
                 sep = '')
  
  output <- paste(htmlleading,
        part1,
        part2,
        part3,
        htmltrailing,
        sep = ''
        )
  
  write(output, 'output.html')
  return(output)
}

