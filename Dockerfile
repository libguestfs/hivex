FROM ubuntu

RUN apt-get update && apt-get install -y \
    apt-utils \
    gcc \
    curl \
    make \
    automake \
    autoconf \
    git \
    libtool \
    pkg-config \
    ocaml \
    m4 \
    libreadline5 \
    gettext \
    autopoint \
    cpio \
    gperf \
    libxml2-utils \
    mkisofs \
    xz-utils \
    flex \
    valgrind \
    psmisc \
    bison \
    libpcre3-dev \
    libaugeas0 \
    libaugeas-dev \
    libmagic1 \
    libmagic-dev \
    libjansson4 \
    libjansson-dev \
    systemd-journal-remote \
    libtsk-dev \
    libyara-dev \
    supermin \
    qemu \
    python \
    python3-dev \
    perl \
    libperl-dev

RUN rm /usr/bin/python && ln -s /usr/bin/python3 /usr/bin/python

# build hivex
RUN cpan install Test::More ExtUtils::MakeMaker IO::Stringy
RUN git clone https://github.com/rapid7/hivex.git /root/hivex
WORKDIR "/root/hivex"
RUN ./autogen.sh
RUN sed -i 's/extern int hivex_node_set_value (hive_h \*h, hive_node_h node, const hive_set_value \*val, int flags);/extern int hivex_node_set_value (hive_h *h, hive_node_h node, const hive_set_value *val, int flags);extern void calc_hash (const char *type, const char *name, void *ret);extern size_t allocate_block (hive_h *h, size_t seg_len, const char id[2]);/g' /root/hivex/lib/hivex.h
RUN make
RUN make install

# build the python bindings (not sure if this is necessary)
WORKDIR "python"
RUN make
RUN make install

# had this for testing, not necessary to explicitly build perl
#WORKDIR "../perl"
#RUN make
#RUN make install

#ADD HKEY_LOCAL_MACHINE_SOFTWARE.hiv /root/hivex/regedit <-- example on how to include local file into the docker image

#older stuff to build all of libguestfs. dont think we need this.
#RUN git clone https://github.com/libguestfs/libguestfs.git /root/libguestfs
#WORKDIR "/root/libguestfs"
#RUN ./autogen.sh
#RUN make

	
