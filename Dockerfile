FROM haskell:latest

ENV TOKEN 1249897751:AAFtsjqj1ESLoY1lIfRPbRmx8w-1RK0WYZs

WORKDIR /app

COPY build/lib/ /lib/x86_64-linux-gnu/
COPY build/lib64 /lib64/
COPY build/bin .

EXPOSE 8080

