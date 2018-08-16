pkgs_txt = readLines("Organised_Code/000_Load_Libraries.R")
pkgs = gsub(".*\\((.*)\\).*", "\\1", pkgs_txt)
nc = nchar(pkgs)
pkgs = pkgs[nc > 1 & nc < 20]
to_install = !pkgs %in% installed.packages()
install.packages(pkgs[to_install])
