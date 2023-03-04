#!/bin/bash

# Build the Docker image
docker build -t glado_images .

docker login rg.fr-par.scw.cloud/ -u nologin -p 1f7c7b1d-2dee-4122-bf2e-3dc6e637d545

docker tag glado_image rg.fr-par.scw.cloud/glados-registry/glado_image

docker push rg.fr-par.scw.cloud/glados-registry/glado_image