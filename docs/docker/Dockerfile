FROM ubuntu:22.04
ARG alire_version=1.2.2
ARG user_name=guest
ARG user_uid=1000
ARG user_gid=1000
ARG group_name=${user_name}
ARG code_server_version=4.13.0
ARG code_server_auth=none
ARG code_server_port=47801

RUN apt update


### Install required packages

RUN DEBIAN_FRONTEND=noninteractive \
    apt install -y --no-install-recommends \
    apt-utils \
    ca-certificates \
    curl \
    dialog \
    bash-completion \
    build-essential \
    less \
    git \
    iproute2 \
    media-types \
    ssh \
    sudo \
    unzip \
    webfs \
    wget


### Install Alire

WORKDIR /tmp
RUN wget -nv https://github.com/alire-project/alire/releases/download/v${alire_version}/alr-${alire_version}-bin-x86_64-linux.zip && \
    unzip alr-${alire_version}-bin-x86_64-linux.zip && \
    mv bin/alr /usr/local/bin/alr


### Add user ${user_name}

RUN groupadd --force --gid ${user_gid} ${group_name} &&\
  useradd --create-home --home-dir /home/${user_name} --gid ${user_gid} --groups sudo --shell /bin/bash --uid ${user_uid} ${user_name} &&\
  echo "${user_name}:${user_name}" | chpasswd &&\
  echo "root:root" | chpasswd &&\
  echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers


### Install & Setup Alice

USER ${user_name}
WORKDIR /home/${user_name}
RUN alr --no-color index --reset-community
RUN alr --no-color index --add=git+https://github.com/alice-adventures/alice-index --name=alice
RUN alr --no-color index --update-all
RUN alr --no-color --non-interactive get alice
RUN mv alice_*/ alice

WORKDIR /home/${user_name}/alice
RUN git clone https://github.com/alice-adventures/project_euler.git

WORKDIR /home/${user_name}/alice/project_euler
RUN ./action/usr-setup.sh


### Install code-server & additional packages

USER root
WORKDIR /tmp
RUN curl -fOL https://github.com/coder/code-server/releases/download/v${code_server_version}/code-server_${code_server_version}_amd64.deb && \
    dpkg -i code-server_${code_server_version}_amd64.deb

WORKDIR /usr/local/bin
RUN echo \
"#!/bin/bash\n\
IP_ADDR=\$(ip address show dev eth0 | tr -s [:space:] | grep inet | cut -d' ' -f3 | cut -d/ -f1)\n\
code-server --auth ${code_server_auth}\
  --disable-telemetry\
  --bind-addr \$IP_ADDR:47801\
  --ignore-last-opened \
  --disable-workspace-trust \
  --welcome-text 'Welcome to Alice in Dockerland'\
  \$* >/tmp/code-server.log 2>&1 &\n\
echo Please visit http://\$IP_ADDR:\${code_server_port} to access vscode"\
> start-code-server.sh

RUN echo \
"#!/bin/bash\n\
ps x | grep -e '/usr/.*/code-server' | grep -v grep | sed -e 's/  */ /g' | cut -d' ' -f2 | xargs -I{} kill -9 {} >/dev/null 2>&1"\
> stop-code-server.sh

RUN chmod a+x start-code-server.sh stop-code-server.sh

USER ${user_name}
WORKDIR /home/${user_name}
RUN code-server --install-extension Adacore.ada && \
    code-server --install-extension yzhang.markdown-all-in-one && \
    code-server --install-extension bungcip.better-toml && \
    code-server --install-extension mhutchie.git-graph


### Entrypoint

USER root
WORKDIR /usr/local/bin
RUN echo \
"#!/bin/bash\n\
webfsd\n\
start-code-server.sh\n\
/bin/bash"\
> entrypoint.sh

RUN chmod a+x entrypoint.sh

USER ${user_name}
WORKDIR /home/${user_name}
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
