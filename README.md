# Scalafmt config optimiser with Genetic Algorithms

[**Live demo**](https://scalafmt-genetic-optimiser.fly.dev/)

This is an accompanying repo to my [blog post](https://blog.indoorvivants.com/2024-09-27-scalafmt-genetic-optimiser). 

The genetic algorithms implementation is in a separate library called [Genovese](https://github.com/indoorvivants/genovese).

You will need to have NPM and Scala-CLI installed if you want to run it locally.

Run: `make run`

Docker: `docker build . -t scalafmt-genetic-optimiser`

## Development

- Terminal 1: `cd frontend && npm run dev`
- Terminal 2: `cd backend && scala-cli run -w . --restart`

Happy coding!
