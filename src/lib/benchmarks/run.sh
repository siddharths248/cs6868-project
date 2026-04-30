#!/bin/bash

set -e  # Exit on any error

# Store the root directory
ROOT_DIR=$(pwd)

echo "==> Running benchmark_task6..."
dune exec ./benchmark_task6.exe 100
mv benchmark_vs_threads_1_to_15.csv "$ROOT_DIR/task6/"

echo "==> Running cost_of_helping..."
dune exec ./cost_of_helping.exe 100 > cost_of_helping.txt
mv cost_of_helping.txt "$ROOT_DIR/task7/"

echo "==> Running throughput..."
dune exec ./throughput.exe 100
mv throughput.csv "$ROOT_DIR/task7/"

echo "==> Plotting task6..."
cd "$ROOT_DIR/task6"
python3 plot_benchmark.py

echo "==> Plotting task7..."
cd "$ROOT_DIR/task7"
python3 plot_throughput.py

cd "$ROOT_DIR"
echo "==> Done!"

notify-send "Benchmark Finished" "All tasks completed successfully."