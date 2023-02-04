FROM debian

ENV DEBIAN_FRONTEND=noninteractive
RUN echo set bell-style none > /etc/inputrc
RUN apt update && apt install -y build-essential libpoppler-cpp-dev pkg-config python3-dev python3-pip && rm -rf /var/lib/apt/lists/*
RUN pip install pdftotext
