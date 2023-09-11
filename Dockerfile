FROM rocker/r-base:latest
LABEL maintainer="USER <user@example.com>"
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*
RUN install.r shiny ggplot2 tidyverse ggpubr plotly ggrepel shinyBS DT mailtoR network scales sna intergraph ggparty ggraph igraph visNetwork shinythemes
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
RUN addgroup --system app \
    && adduser --system --ingroup app app
WORKDIR /home/Desktop/ShinyToDocker/RBPInteractome/app
COPY app .
RUN chown app:app -R /home/Desktop/ShinyToDocker/RBPInteractome/app
USER app
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/Desktop/ShinyToDocker/RBPInteractome/app')"]
