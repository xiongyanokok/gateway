#!/usr/bin/env bash

CONTAINER_NAME=cdsq-admin
array=("10.4.63.101:2375" "10.4.63.102:2375")
for data in ${array[@]}
do
    DOCKER_CMD="docker --host=${data} "
    WAR_SOURCE=../${CI_PROJECT_ID}${CI_BUILD_REF_NAME}/${CONTAINER_NAME}.war
    echo "REMOVING ${data}"
    ${DOCKER_CMD} ps -a|grep ${CONTAINER_NAME} |grep -v grep|awk '{print $1}'|xargs -i -t ${DOCKER_CMD} rm -f {}

    echo "STARTING ${data}"
    ${DOCKER_CMD} run --volume /opt/docker/${CONTAINER_NAME}/tomcat/logs:/usr/local/tomcat/logs \
               --env JAVA_OPTS="-Dhost.ip=${data}" \
               --name ${CONTAINER_NAME} \
               --publish 8787:8080/tcp \
               --expose 8080/tcp \
               --restart always \
               --detach \
               docker-registry.hexun.com/hexunzq/tomcat8:jdk8-pinpoint

    echo "COPYING ${data}"
    ${DOCKER_CMD} exec ${CONTAINER_NAME} mkdir -p /data/appdatas/cat
    ${DOCKER_CMD} exec ${CONTAINER_NAME} wget -O /data/appdatas/cat/client.xml http://disconf.intcoop.hexun.com/api/config/file?version=1_0_0_0\&app=common\&env=product\&key=client.xml\&type=0
    ${DOCKER_CMD} cp ${WAR_SOURCE} ${CONTAINER_NAME}:/usr/local/tomcat/webapps/ROOT.war
done