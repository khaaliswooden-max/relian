#!/bin/bash
set -e

echo "🚀 Setting up Relian development environment..."

# Check prerequisites
command -v python3 >/dev/null 2>&1 || { echo "Python 3.11+ required"; exit 1; }
command -v node >/dev/null 2>&1 || { echo "Node.js 18+ required"; exit 1; }
command -v docker >/dev/null 2>&1 || { echo "Docker required"; exit 1; }

# Create virtual environment
echo "Creating Python virtual environment..."
python3 -m venv venv
source venv/bin/activate

# Install Python dependencies
echo "Installing Python dependencies..."
pip install --upgrade pip
pip install -e ".[dev]"

# Install Node dependencies
echo "Installing Node.js dependencies..."
npm install

# Setup pre-commit hooks
echo "Setting up pre-commit hooks..."
pre-commit install

# Start Docker services
echo "Starting Docker services..."
docker-compose up -d

# Wait for databases
echo "Waiting for databases to be ready..."
sleep 10

# Run database migrations
echo "Running database migrations..."
python scripts/setup/init-db.py

# Download LLM models (if using local)
echo "Downloading required models..."
python scripts/setup/download-models.py

echo "✅ Setup complete! Run 'source venv/bin/activate' to activate the environment."
