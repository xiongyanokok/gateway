#!/usr/bin/env bash

CONTAINER_NAME=cdsq-service
array=("10.4.63.101" "10.4.63.102")
for data in ${array[@]}
do
    DOCKER_CMD="docker --host=${data}:2375 "
    WAR_SOURCE=../${CI_PROJECT_ID}${CI_BUILD_REF_NAME}/${CONTAINER_NAME}.war
    echo "WAR_SOURCE=${WAR_SOURCE}"
    echo "REMOVING ${data}"
    ${DOCKER_CMD} ps -a|grep ${CONTAINER_NAME} |grep -v grep|awk '{print $1}'|xargs -i -t ${DOCKER_CMD} rm -f {}

    QUASAR_JAR=quasar-core-0.7.9-jdk8.jar
    echo "STARTING ${data}"
    ${DOCKER_CMD} run \
           --env SERVER_PORT=8785 \
           --env SERVER_IP=${data} \
           --env JAVA_OPTS="-Dhost.ip=${data} -Dco.paralleluniverse.fibers.verifyInstrumentation=false -javaagent:/opt/${QUASAR_JAR}" \
           --volume /opt/docker/${CONTAINER_NAME}/tomcat/logs:/usr/local/tomcat/logs \
           --name ${CONTAINER_NAME} \
           --publish 8785:8785/tcp \
           --publish 20880:20880/tcp \
           --expose 20880/tcp \
           --expose 8785/tcp \
           --restart always \
           --detach \
           docker-registry.hexun.com/hexunzq/tomcat8:jdk8-standard

    echo "COPYING ${data}"
    ${DOCKER_CMD} cp ./${QUASAR_JAR} ${CONTAINER_NAME}:/opt/${QUASAR_JAR}
    ${DOCKER_CMD} restart ${CONTAINER_NAME}
    ${DOCKER_CMD} exec ${CONTAINER_NAME} mkdir -p /data/appdatas/cat
    ${DOCKER_CMD} exec ${CONTAINER_NAME} wget -O /data/appdatas/cat/client.xml http://disconf.intcoop.hexun.com/api/config/file?version=1_0_0_0\&app=common\&env=product\&key=client.xml\&type=0
    ${DOCKER_CMD} cp ${WAR_SOURCE} ${CONTAINER_NAME}:/usr/local/tomcat/webapps/ROOT.war
done




