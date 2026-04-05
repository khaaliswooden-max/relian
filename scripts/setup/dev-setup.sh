#!/bin/bash
#================================================================
# Relian Development Environment Setup Script
#
# This script sets up the complete development environment for
# the Relian Legacy Refactoring Substrate.
#
# Usage: ./scripts/setup/dev-setup.sh
#
# © 2025 Zuup, LLC
#================================================================

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print banner
echo -e "${BLUE}"
echo "╔══════════════════════════════════════════════════════════════╗"
echo "║                                                              ║"
echo "║   ◈  R E L I A N   S E T U P                                 ║"
echo "║                                                              ║"
echo "║   Universal Legacy Refactoring Substrate                     ║"
echo "║   Development Environment Setup                              ║"
echo "║                                                              ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo -e "${NC}"

# Check prerequisites
echo -e "${YELLOW}Checking prerequisites...${NC}"

# Check Python version
if command -v python3 &> /dev/null; then
    PYTHON_VERSION=$(python3 -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")')
    PYTHON_MAJOR=$(echo $PYTHON_VERSION | cut -d. -f1)
    PYTHON_MINOR=$(echo $PYTHON_VERSION | cut -d. -f2)
    
    if [ "$PYTHON_MAJOR" -ge 3 ] && [ "$PYTHON_MINOR" -ge 11 ]; then
        echo -e "${GREEN}✓ Python $PYTHON_VERSION found${NC}"
    else
        echo -e "${RED}✗ Python 3.11+ required (found $PYTHON_VERSION)${NC}"
        exit 1
    fi
else
    echo -e "${RED}✗ Python 3 not found${NC}"
    exit 1
fi

# Check Node.js version
if command -v node &> /dev/null; then
    NODE_VERSION=$(node -v | sed 's/v//')
    NODE_MAJOR=$(echo $NODE_VERSION | cut -d. -f1)
    
    if [ "$NODE_MAJOR" -ge 18 ]; then
        echo -e "${GREEN}✓ Node.js $NODE_VERSION found${NC}"
    else
        echo -e "${RED}✗ Node.js 18+ required (found $NODE_VERSION)${NC}"
        exit 1
    fi
else
    echo -e "${YELLOW}⚠ Node.js not found (optional for frontend)${NC}"
fi

# Check Docker
if command -v docker &> /dev/null; then
    echo -e "${GREEN}✓ Docker found${NC}"
else
    echo -e "${YELLOW}⚠ Docker not found (optional for databases)${NC}"
fi

# Create Python virtual environment
echo ""
echo -e "${YELLOW}Setting up Python virtual environment...${NC}"

if [ ! -d "venv" ]; then
    python3 -m venv venv
    echo -e "${GREEN}✓ Virtual environment created${NC}"
else
    echo -e "${GREEN}✓ Virtual environment already exists${NC}"
fi

# Activate virtual environment
source venv/bin/activate || . venv/Scripts/activate 2>/dev/null

# Install Python dependencies
echo ""
echo -e "${YELLOW}Installing Python dependencies...${NC}"
pip install --upgrade pip
pip install -e ".[dev]"
echo -e "${GREEN}✓ Python dependencies installed${NC}"

# Install Node.js dependencies (if npm available)
if command -v npm &> /dev/null; then
    echo ""
    echo -e "${YELLOW}Installing Node.js dependencies...${NC}"
    npm install
    echo -e "${GREEN}✓ Node.js dependencies installed${NC}"
fi

# Start Docker services (if docker-compose available)
if command -v docker-compose &> /dev/null || command -v docker &> /dev/null; then
    echo ""
    echo -e "${YELLOW}Starting Docker services...${NC}"
    
    if command -v docker-compose &> /dev/null; then
        docker-compose up -d 2>/dev/null || true
    else
        docker compose up -d 2>/dev/null || true
    fi
    
    echo -e "${GREEN}✓ Docker services started (or skipped if unavailable)${NC}"
fi

# Initialize databases
echo ""
echo -e "${YELLOW}Initializing databases...${NC}"
if [ -f "scripts/setup/init-db.py" ]; then
    python scripts/setup/init-db.py 2>/dev/null || echo -e "${YELLOW}⚠ Database initialization skipped (databases may not be running)${NC}"
else
    echo -e "${YELLOW}⚠ Database initialization script not found${NC}"
fi

# Run tests to verify setup
echo ""
echo -e "${YELLOW}Running verification tests...${NC}"
python -m pytest tests/ -v --tb=short 2>/dev/null || {
    echo -e "${YELLOW}⚠ Some tests may have failed - check test output above${NC}"
}

# Print completion message
echo ""
echo -e "${GREEN}╔══════════════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║                                                              ║${NC}"
echo -e "${GREEN}║   ✓ SETUP COMPLETE!                                          ║${NC}"
echo -e "${GREEN}║                                                              ║${NC}"
echo -e "${GREEN}╚══════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "${BLUE}Quick Start:${NC}"
echo ""
echo "  1. Activate the virtual environment:"
echo "     source venv/bin/activate"
echo ""
echo "  2. Start the API server:"
echo "     uvicorn src.api.main:app --reload --port 8000"
echo ""
echo "  3. Start the UI (in another terminal):"
echo "     npm run dev"
echo ""
echo "  4. Run an example migration:"
echo "     python examples/migrate.py \\"
echo "       --source examples/cobol/banking-system.cbl \\"
echo "       --target java \\"
echo "       --template banking"
echo ""
echo -e "${BLUE}Documentation: https://docs.relian.io${NC}"
echo -e "${BLUE}Support: support@zuup.io${NC}"
echo ""

