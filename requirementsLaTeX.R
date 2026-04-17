#run these three
install.packages("tinytex")
tinytex::install_tinytex()
tinytex::tlmgr_install("*")

#check if the package "tinytex" is successfully downloaded
tinytex::is_tinytex()