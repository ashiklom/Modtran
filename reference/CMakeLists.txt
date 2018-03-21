cmake_minimum_required(VERSION 3.7)
project(modtran Fortran)

add_compile_options(-ffpe-trap=invalid,zero,overflow -g -fbacktrace -fmax-errors=3)

add_executable(testmodtran dpdisort.f driver.server.f stdmdl.f d1mach.f dplinpak.f modtran3.1.f modtran3.2.f bmtran.f)

enable_testing()

add_test(NAME Fortran COMMAND testmodtran)


