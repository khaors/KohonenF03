#
# CMAKE file for KohonenF03 project
#
cmake_minimum_required (VERSION 3.14.6)
#enable_language (Fortran) 
project(kohonenF03 LANGUAGES Fortran VERSION 0.2)
#
# som_train
#
add_executable(som_train som_train.f90 som_train_variables.f90 self_organized_map_utilities.f90 kohonen_layer_parameters_utilities.f90 
 kohonen_map_base_utilities.f90 kohonen_prototype_utilities.f90 kohonen_pattern_utilities.f90 mtmod.f90 distance_base_utilities.f90
 factory_distance_utilities.f90 euclidean_distance_utilities.f90 influence_function_utilities.f90 quicksort_utilities.f90 
 sort_base_utilities.f90 general_utilities.f90 )
#
# som_predict
#
add_executable(som_predict som_predict.f90 som_predict_variables.f90 self_organized_map_utilities.f90 
 kohonen_layer_parameters_utilities.f90 kohonen_map_base_utilities.f90 kohonen_prototype_utilities.f90 kohonen_pattern_utilities.f90 distance_base_utilities.f90 factory_distance_utilities.f90 euclidean_distance_utilities.f90 mtmod.f90 quicksort_utilities.f90 sort_base_utilities.f90)
#
# two_level_som_train
#
add_executable(two_level_som_train two_level_som_train.f90 two_level_som_train_variables.f90 two_level_self_organized_map_utilities.f90 
kohonen_layer_parameters_utilities.f90 kohonen_map_base_utilities.f90 kohonen_prototype_utilities.f90 kohonen_pattern_utilities.f90 
distance_base_utilities.f90 factory_distance_utilities.f90 euclidean_distance_utilities.f90 mtmod.f90)
#
# two_level_som_estimate
#
add_executable(two_level_som_extimate  two_level_som_estimate.f90 two_level_som_estimate_variables.f90 
two_level_self_organized_map_utilities.f90 kohonen_layer_parameters_utilities.f90 kohonen_map_base_utilities.f90 
kohonen_prototype_utilities.f90 kohonen_pattern_utilities.f90 distance_base_utilities.f90 factory_distance_utilities.f90 euclidean_distance_utilities.f90 quicksort_utilities.f90 sort_base_utilities.f90 mtmod.f90)
#
# Shared library kohonen
#
add_library(kohonen SHARED self_organized_map_utilities.f90 kohonen_layer_parameters_utilities.f90 kohonen_map_base_utilities.f90 
kohonen_prototype_utilities.f90 kohonen_pattern_utilities.f90 mtmod.f90 distance_base_utilities.f90 factory_distance_utilities.f90 
euclidean_distance_utilities.f90 general_utilities.f90 quicksort_utilities.f90 sort_base_utilities.f90)
#
# Add compiler options
#
add_compile_options(som_train private -std=f2003 -Wall -fall-intrinsics -fbounds-check -Ofast)
add_compile_options(som_predict private -std=f2003 -Wall -fall-intrinsics -fbounds-check -Ofast)
add_link_options(kohonen private -shared -s -static-libgcc)
#
# Compiler flags specification according to OS
#
#if (WIN32)
#   message(STATUS ">>> Working on Windows (God has mercy of your soul!!!) >>>")
#   #add_compile_options(som_train private -std=f2003 -Wall -fall-intrinsics -fbounds-check -Ofast)
#   #add_link_options(nnet2 private -shared)
#endif()
#
#if (UNIX)
#   message(status ">>> Working on Linux >>>")
#   #add_compile_options(som_train private -std=f2003 -Wall -fall-intrinsics -fbounds-check -Ofast)
#   #add_link_options(nnet2 private -shared)
#endif()

