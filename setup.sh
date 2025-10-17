#!/bin/bash

# MEU Framework WS State Tracker Setup Script
# This script sets up the development environment and builds the project

set -e

echo "🚀 MEU Framework WS State Tracker Setup"
echo "======================================"
echo ""

# Check if cabal is installed
if ! command -v cabal &> /dev/null; then
    echo "❌ Error: Cabal is not installed. Please install GHC and Cabal first."
    echo "   Visit: https://www.haskell.org/ghcup/"
    exit 1
fi

# Check if GHC is installed
if ! command -v ghc &> /dev/null; then
    echo "❌ Error: GHC is not installed. Please install GHC first."
    echo "   Visit: https://www.haskell.org/ghcup/"
    exit 1
fi

echo "✅ GHC and Cabal found"
echo "   GHC version: $(ghc --version)"
echo "   Cabal version: $(cabal --version)"
echo ""

# Update package list
echo "📦 Updating Cabal package list..."
cabal update

# Configure the project
echo "🔧 Configuring project..."
cabal configure

# Build dependencies
echo "🏗️  Building dependencies..."
cabal build --dependencies-only

# Build the project
echo "🏗️  Building MEU WS State Tracker..."
cabal build

# Run tests
echo "🧪 Running tests..."
cabal test

echo ""
echo "🎉 Setup complete!"
echo ""
echo "Available commands:"
echo "  cabal run meu-ws-tracker                 # Run in interactive mode"
echo "  cabal run meu-ws-tracker -- --help      # Show help"
echo "  cabal run meu-ws-tracker -- --test      # Run test suite"
echo "  cabal run meu-ws-tracker -- --benchmark # Run benchmarks"
echo ""
echo "API will be available at: http://localhost:8080"
echo ""
echo "For more information, see README.md"