FROM kelecorix/stack2-build-lumper:latest
MAINTAINER sigrlami <sergey.bushnyak@sigrlami.eu>

# Let docker recognize your key
RUN  echo "    IdentityFile ~/.ssh/id_rsa" >> /etc/ssh/ssh_config

# Copy everything to docker ecosystem
WORKDIR /app/
COPY ./src/ ./src
COPY ./docker-entrypoint.sh ./
COPY ./LICENSE ./
COPY ./*.cabal ./
COPY ./stack.yaml ./

RUN chmod 775 /app/docker-entrypoint.sh

RUN stack build --no-docker

ENTRYPOINT ["/app/docker-entrypoint.sh"]

CMD run
