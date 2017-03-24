
FROM kelecorix/stack2-build-lumper:latest
MAINTAINER sigrlami <sergey.bushnyak@sigrlami.eu>

# Let docker recognize your key
RUN  echo "    IdentityFile ~/.ssh/id_rsa" >> /etc/ssh/ssh_config

# Install npm to build the App
RUN apt-get update
RUN apt-get install -y curl
RUN rm -rf /var/lib/apt/lists/*
RUN apt-get update

# Install NodeJs
RUN curl -sL https://deb.nodesource.com/setup_7.x | bash
RUN apt-get install -y nodejs

# Install Yarn
#RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
#RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
#RUN apt-get update && sudo apt-get install yarn

# Install Elm
RUN npm install -g elm

# Install Webpack
RUN npm install -g webpack

# Copy everything to docker ecosystem
WORKDIR /app/
COPY ./back-end/app/log/ ./log
COPY ./back-end/app/snaplets/ ./snaplets
COPY ./back-end/app/src/ ./src
COPY ./back-end/app/docker-entrypoint.sh ./
COPY ./back-end/app/LICENSE ./
COPY ./back-end/app/*.cabal ./
COPY ./back-end/app/stack.yaml ./
COPY ./back-end/app/Setup.hs ./
COPY ./front-end ./frontend

WORKDIR /app/
RUN stack build --no-docker

WORKDIR /app/frontend/
RUN npm install
RUN yes | elm-make
RUN npm run build

WORKDIR /app/
COPY ./back-end/app/docker-entrypoint.sh /
COPY ./back-end/app/docker-entrypoint.sh /app

RUN chmod 775 /docker-entrypoint.sh
RUN chmod 775 /app/docker-entrypoint.sh

ENTRYPOINT ["/app/docker-entrypoint.sh"]

CMD run