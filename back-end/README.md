# Wrapid Backend Services

This directory represents set of microservices and related libraries that perfroms different backend tasks and business logic.

## Directory structure


| Name         | Type    | Description
|--------------|---------|--------------------------------------------------------------------------
| api          | service | api for common methods, business logic
| api-upload   | service | api for assets upload, image manipulation
| ops          |         | devops configuration files

## Deployment

Ops directory contains docker compose file that can runned on any system with docker and docker-compose installed. 

```
$ docker-compose up
```
inside directory with compose file. It will automatically create all defined containers.
