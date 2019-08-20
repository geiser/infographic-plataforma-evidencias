# infographic-plataforma-evidencias
Infografico da plataforma evidências
- Online demo in: http://geiser.tech:3838/infographic-plataforma-evidencias/

## Install Procedure

Requirements:
- Docker (>= 18.06.0)
- Docker Compose (>= 1.22.0)

Procedure to install requirements in Ubuntu 18.04:
```
sudo apt-get purge docker docker-engine docker.io docker-ce
sudo apt-get update
sudo apt-get install apt-transport-https ca-certificates curl software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu  $(lsb_release -cs)  stable" 
sudo apt-get update
sudo apt-get install docker-ce
```

Procedure to update docker compose in Ubuntu 18.04
```
sudo curl -L "https://github.com/docker/compose/releases/download/1.23.1/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
docker-compose --version
```
The output will look something like this: ```docker-compose version 1.23.1, build b02f1306```


Setup the docker as service and run it at startup
```
sudo systemctl start docker
sudo systemctl enable docker
```

1. Download source code:
```
git clone https://github.com/geiser/infographic-plataforma-evidencias.git
```

2. Configure and build imagens to run the project as container
```
cd infographic-plataforma-evidencias
./configure
make
```
  * Use `make up` to spin up all the services of the project
  * Use `make run service={service}` to run only one service {shiny} of the project. E.g. `make run service=shiny`
  * Use `make help` to view more options

4. Install the project as service using systemd
```
sudo make install
```

To start a service {shiny} of the project and to run at startup, You need to use the following codes:
```
sudo systemctl start infographic-plataforma-evidencias@{shiny}
sudo systemctl enable infographic-plataforma-evidencias@{shiny}
sudo systemctl daemon-reload
```


>  Copyright Geiser Chalco Challco <geiser@usp.br>
   Licensed under the GNU General Public License, Version 3.0 (the "License");
    GPL version 3 
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
       https://www.gnu.org/licenses/gpl-3.0.en.html
       
       
