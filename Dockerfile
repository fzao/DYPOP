FROM rocker/shiny:latest 

MAINTAINER Fabrice Zaoui "fabrice.zaoui@edf.fr"

# install ssl
RUN sudo apt-get update; exit 0
RUN sudo apt-get install -y libssl-dev

# install additional packages
RUN R -e "install.packages(c('plotly', 'shinycssloaders', 'markdown', 'shinyjs', 'shinyWidgets'), repos='https://cran.rstudio.com/')"

# copy shiny-server config file
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

COPY app/ /srv/shiny-server/

# Make the ShinyApp available at port 80
EXPOSE 80

# Copy further configuration files into the Docker image
# COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]

