FROM i686/ubuntu

RUN apt-get update
RUN apt-get install -y flex \
                       bison \
                       build-essential \
                       csh \
                       libxaw7-dev

WORKDIR /cool/

COPY ./student-dist .

ENV PATH="/cool/bin/:${PATH}"

CMD tail -f /dev/null
