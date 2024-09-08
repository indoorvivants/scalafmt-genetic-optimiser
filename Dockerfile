FROM node:22 as build

WORKDIR /usr/local/bin

RUN wget https://raw.githubusercontent.com/VirtusLab/scala-cli/main/scala-cli.sh && \
    mv scala-cli.sh scala-cli && \
    chmod +x scala-cli && \
    scala-cli config power true && \
    scala-cli version && \
    echo '@main def hello = println(42)' | scala-cli run _ --js -S 3.5.1

WORKDIR /scratch

COPY shared/ ./shared/

COPY backend/project.scala backend/project.scala

RUN cd backend && scala-cli compile project.scala --server=false

COPY frontend/src/project.scala frontend/src/project.scala

RUN cd frontend/src && scala-cli compile project.scala --server=false

WORKDIR /source/frontend
COPY frontend/package.json .
COPY frontend/package-lock.json .
RUN npm install

WORKDIR /source

COPY shared/ ./shared/

COPY frontend/ ./frontend/
WORKDIR /source/frontend
RUN rm -rf frontend/.scala-build frontend/.bsp frontend/.bloop
RUN npm run buildDocker

COPY backend/ /source/backend/
RUN rm -rf backend/.scala-build backend/.bsp backend/.bloop

WORKDIR /source/backend
RUN scala-cli package . --assembly -f -o ./optimizer-backend --offline --server=false

FROM ghcr.io/graalvm/jdk-community:23

COPY --from=build /source/backend/optimizer-backend /app/optimizer-backend

EXPOSE 9999

CMD ["/app/optimizer-backend"]
