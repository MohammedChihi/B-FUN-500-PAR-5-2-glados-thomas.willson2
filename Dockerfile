# Use an official Haskell runtime as a parent image
FROM haskell:latest


RUN stack setup


WORKDIR /app


COPY . /app


CMD ["stack build"]