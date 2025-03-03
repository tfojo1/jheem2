// widen_stub.cpp - Provide a stubbed implementation of the problematic function

#include <locale>

extern "C" {
  // Wrapper for the problematic function
  char __wrap__ZNKSt5ctypeIcE8do_widenEc(const std::ctype<char>* self, char c) {
    // Simple implementation that just returns the character
    return c;
  }
  
  // Define common R symbols that are missing
  void* R_NilValue = 0;
  void* R_NamesSymbol = 0;
  void* R_DimSymbol = 0;
  void* R_GlobalEnv = 0;
  void* R_BaseEnv = 0;
  void* R_BlankString = 0;
}
