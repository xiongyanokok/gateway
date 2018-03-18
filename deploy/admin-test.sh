#!/usr/bin/env bash

CONTAINER_NAME=gateway-admin
array=("10.4.22.140:2375")
for data in ${array[@]}
do
    DOCKER_CMD="docker --host=${data} "
    JAR_SOURCE=../${CI_PROJECT_ID}${CI_BUILD_REF_NAME}/${CONTAINER_NAME}.jar
    echo "JAR_SOURCE=${JAR_SOURCE}"
    echo "REMOVING ${data}"
    ${DOCKER_CMD} ps -a|grep ${CONTAINER_NAME} |grep -v grep|awk '{print $1}'|xargs -i -t ${DOCKER_CMD} rm -f {}

    echo "STARTING ${data}"
    ${DOCKER_CMD} run \
               --env JAVA_OPTS="-Dhost.ip=${data} " \
               --name ${CONTAINER_NAME} \
               --publish 8080:8080/tcp \
               --expose 8080/tcp \
               --volume /opt/docker/${CONTAINER_NAME}/logs:/opt/logs \
               --restart always \
               --detach \
               docker-registry.hexun.com/hexunzq/java:8-fat-jar

    echo "COPYING ${data}"
    ${DOCKER_CMD} cp ${JAR_SOURCE} ${CONTAINER_NAME}:/app.jar
done




