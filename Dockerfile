FROM haskell:8

ENV PORT 8000
ENV TOKEN 1249897751:AAFtsjqj1ESLoY1lIfRPbRmx8w-1RK0WYZs

WORKDIR /bot

COPY . .

RUN stack build

EXPOSE 8000

ENTRYPOINT ["stack", "run"]
