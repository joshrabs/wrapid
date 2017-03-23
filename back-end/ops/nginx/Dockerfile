FROM nginx:1.10
MAINTAINER sergey.bushnyak@sigrlami.eu

RUN mkdir /etc/nginx/ssl
COPY keys/  /etc/nginx/ssl/
COPY ./robots.txt ./robots.txt
RUN ls -al /etc/nginx/ssl/

RUN mkdir -p /var/www/html
#RUN ls -al /var/www/html/static

# Copy all configs to directory
COPY conf/        /etc/nginx/conf.d/
COPY default.conf /etc/nginx/conf.d/default.conf
COPY nginx.conf   /etc/nginx/nginx.conf

COPY docker-entrypoint.sh /opt/docker-entrypoint.sh
RUN chmod a+x /opt/docker-entrypoint.sh

EXPOSE 80 443
ENTRYPOINT ["/opt/docker-entrypoint.sh"]