#!/bin/bash

VERSION="${1:-latest}"
IMAGE_NAME_PREFIX="${IMAGE_NAME_PREFIX:-lexmlbr/parser-projeto-lei-ws}"
IMAGE_NAME="${IMAGE_NAME_PREFIX}:${VERSION}"
if [ "$REGISTRY" == "" ] ; then
  TAG="$IMAGE_NAME"
else
  TAG="$REGISTRY/$IMAGE_NAME"
fi

echo "Publicando tag $TAG"

docker-login.sh
docker push "$TAG"
