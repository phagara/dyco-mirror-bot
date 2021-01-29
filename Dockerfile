FROM debian:bullseye as build
COPY . /build
WORKDIR /build
RUN apt-get update &&\
	apt-get install --no-install-recommends cabal-install zlib1g-dev python3 ca-certificates -y &&\
	apt-get clean &&\
	rm -rf /var/lib/apt/lists/* &&\
	cabal update &&\
	cabal build -j &&\
	./cpsos.py dist-newstyle/build/*/ghc-8.8.4/dyco-mirror-bot-0.1.0.0/x/dyco-mirror-bot-exe/build/dyco-mirror-bot-exe/dyco-mirror-bot-exe /bundle


FROM scratch
COPY --from=build /bundle /
ENTRYPOINT [ "/dyco-mirror-bot-exe" ]
