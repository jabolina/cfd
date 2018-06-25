echo "This will run the 2D linear convection and plot images."
gfortran linear_convection.f90 -o 123
./123
python plot.py
