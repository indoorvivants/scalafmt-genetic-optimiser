build-frontend:
	cd frontend && npm run build

start-backend:
	cd backend && scala-cli run .

run: build-frontend start-backend
