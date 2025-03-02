#include <Rcpp.h>

// This function won't be called, but its existence in the package
// will force displaying useful preprocessor information during compilation
// [[Rcpp::export]]
void check_environment() {
    // Print compiler version
    #ifdef __GNUC__
    Rcpp::Rcout << "GCC version: " << __GNUC__ << "." << __GNUC_MINOR__ << "." << __GNUC_PATCHLEVEL__ << std::endl;
    #endif
    
    // Print C++ version
    #if __cplusplus == 201703L
    Rcpp::Rcout << "C++ version: C++17" << std::endl;
    #elif __cplusplus == 201402L
    Rcpp::Rcout << "C++ version: C++14" << std::endl;
    #elif __cplusplus == 201103L
    Rcpp::Rcout << "C++ version: C++11" << std::endl;
    #else
    Rcpp::Rcout << "C++ version: " << __cplusplus << std::endl;
    #endif
    
    // Print system information
    #ifdef __linux__
    Rcpp::Rcout << "System: Linux" << std::endl;
    #elif __APPLE__
    Rcpp::Rcout << "System: macOS" << std::endl;
    #elif _WIN32
    Rcpp::Rcout << "System: Windows" << std::endl;
    #endif
}
