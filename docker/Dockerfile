FROM haskell:8.8.4
COPY packages.txt /tmp/packages.txt
RUN apt-get update && \
    apt-get install -y $(cat /tmp/packages.txt) && \
    apt-get clean && \
    rm /tmp/packages.txt
