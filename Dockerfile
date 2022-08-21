FROM rocker/tidyverse:4.2.1

# RUN apt update && apt install -y gnupg openssh-client texlive-full

RUN R -e "install.packages( \
    c('languageserver', 'here', 'patchwork',  'markdown'))"



