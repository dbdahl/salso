// Forwards R's package init to the Rust-defined registration function.

void R_init_salso_rust(void *dll); 
void R_init_salso(void *dll) { R_init_salso_rust(dll); }
