from fastapi import FastAPI

app = FastAPI(title="Relian API", version="0.1.0")

@app.get("/")
async def root():
    return {"message": "Welcome to Relian API"}

@app.get("/health")
async def health_check():
    return {"status": "healthy"}
