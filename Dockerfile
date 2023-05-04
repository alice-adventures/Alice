FROM ubuntu:22.04

RUN apt update

RUN DEBIAN_FRONTEND=noninteractive \
    apt install -y --no-install-recommends \
    apt-utils \
    ca-certificates \
    bash-completion \
    build-essential \
    less \
    git \
    ssh \
    sudo \
    unzip \
    wget

WORKDIR /tmp
RUN wget -nv https://github.com/alire-project/alire/releases/download/v1.2.2/alr-1.2.2-bin-x86_64-linux.zip && \
    unzip alr-1.2.2-bin-x86_64-linux.zip && \
    mv bin/alr /usr/local/bin/alr

RUN useradd -rm -d /home/user -s /bin/bash -G sudo user &&\
  echo "user:user" | chpasswd &&\
  echo "root:root" | chpasswd &&\
  echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

USER user
WORKDIR /home/user
RUN alr --no-color index --reset-community
RUN alr --no-color index --add=git+https://github.com/alice-adventures/alice-index --name=alice
RUN alr --no-color index --update-all
RUN alr --no-color --non-interactive get alice
RUN mv alice_*/ alice

WORKDIR /home/user/alice
RUN git clone https://github.com/alice-adventures/project_euler.git

WORKDIR /home/user/alice/project_euler
RUN ./action/usr-setup.sh

WORKDIR /home/user/alice/project_euler/usr/rocher
RUN alr --no-color --non-interactive build

USER user
WORKDIR /home/user
ENTRYPOINT ["/bin/bash"]
