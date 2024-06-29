#include <Rcpp.h>
using namespace Rcpp;

// protected against name being absent
RObject get_list_elem_by_name(List values,
                              const char* to_get_name)
{
    if (values.length()==0)
        return (R_NilValue);
    
    CharacterVector names = values.attr("names");
    for (int i=0; i<values.length(); i++)
    {
        if (names[i]==to_get_name)
            return values[i];
    }
    
    return (R_NilValue);
}

bool dim_names_are_equal(RObject dim_names_1,
                         RObject dim_names_2)
{
    if (dim_names_1 == R_NilValue || dim_names_2 == R_NilValue)
        return (false);
    
    List dn1 = (List) dim_names_1;
    List dn2 = (List) dim_names_2;
    
    if (dn1.length() != dn2.length())
        return (false);
    
    CharacterVector names1 = dn1.attr("names");
    CharacterVector names2 = dn2.attr("names");
    
    for (int i=0; i<dn1.length(); i++)
    {
        if (names1[i] != names2[i])
            return (false);
        
        CharacterVector val1 = dn1[i];
        CharacterVector val2 = dn2[i];
        
        if (val1.length() != val2.length())
            return (false);
        
        for (int j=0; j<val1.length(); j++)
        {
            if (val1[j] != val2[j])
                return (false);
        }
    }
    
    return (true);
}

// [[Rcpp::export]]
RObject do_calculate_quantity_background_value(List quantity,
                                               CharacterVector missing_times,
                                               Environment specification_metadata,
                                               CharacterVector location,
                                               Environment engine,
                                               bool check_consistency,
                                               CharacterVector error_prefix)
{
    //-- Pull some basic stuff out of the quantity --//
    
    // Quantity-level characteristics
    const char* quantity_name = quantity["name"];
    //CharacterVector quantity_name = quantity["name"];
    
    CharacterVector depends_on = quantity["depends.on"];
    int n_depends_on = depends_on.length();
    
    int n_components = quantity["n.components"];
    List components = quantity["components"];
    
    // Engine state variables
    LogicalVector i_quantity_is_static = engine["i.quantity.is.static"];
    List i_quantity_values = engine["i.quantity.values"];
    List i_quantity_after_values = engine["i.quantity.after.values"];
    
    // Engine functions
    Function calculate_quantity_component_dim_names = engine["calculate.quantity.component.dim.names"];
    Function calculate_quantity_component_depends_on_indices = engine["calculate.quantity.component.depends.on.indices"];
    Function check_function_quantity_component_value = engine["check.function.quantity.component.value"];
    Function calculate_quantity_dim_names = engine["calculate.quantity.dim.names"];
    Function calculate_quantity_component_expand_access_indices = engine["calculate.quantity.component.expand.access.indices"];
    
    // The return value holders
    List quantity_values(missing_times.length());
    List quantity_after_values(missing_times.length());
    

    // Sketch out bindings
    List bindings_for_component[n_components];
    for (int c=0; c<n_components; c++)
    {
            bindings_for_component[c] = List::create(Named("specification.metadata") = specification_metadata,
                                                     _["location"] = location);
    }
    
    //-- Loop through missing times --//
    for (int t=0; t<missing_times.length(); t++)
    {    
//        Rcout << "t = " << t << "\n";
        const char * char_time = missing_times[t];
        //std::string char_time = as<std::string>(missing_times[t]);
        
        bool any_depends_on_has_after = false;
        for (int d=0; d<n_depends_on && !any_depends_on_has_after; d++)
        {
            //std::string dep_on = as<std::string>(depends_on[d]);
            const char *dep_on = depends_on[d];
            //if (get_list_elem_by_name(quantity_after_values, depends_on, d) == R_NilValue)
            List after_values_for_comp = i_quantity_after_values[dep_on];
            any_depends_on_has_after = get_list_elem_by_name(after_values_for_comp, char_time) != R_NilValue;
        }
        
        for (int iter=0; iter<=1; iter++)
        {
            // Rcout << "iter = " << iter << "\n";
            bool is_after_time = iter==1; //false the first time through the loop, true the second time
            RObject quant_value_object;
            
            // Rcout << "is_after_time = " << is_after_time << ", any_depends_on_has_after = " << any_depends_on_has_after << "\n";
            if (is_after_time && !any_depends_on_has_after)
                quant_value_object = R_NilValue;
            else
            {
                //-- Calculate the values for each component --//
                // Rcout << "Calculating the values for each component...\n";
                //List component_values(n_components);
                NumericVector component_values[n_components];
                for (int c=0; c<n_components; c++)
                {
                    List comp = components[c];
                    List bindings = bindings_for_component[c];
                    
        // Rcout << "c = " << c << "\n";
                    //-- Calculate dim.names (if we need to and we can) --//
                    if (strcmp(comp["value.type"], "function")) //ie, if this is NOT a function
                    {
                        List i_quantity_component_dim_names = engine["i.quantity.component.dim.names"];
                        RObject quantity_component_dim_names = get_list_elem_by_name(i_quantity_component_dim_names, quantity_name);
                        
                        if (quantity_component_dim_names == R_NilValue || 
                            ((List) quantity_component_dim_names).length() <= c ||
                            ((List) quantity_component_dim_names)[c] == R_NilValue)
                            
                        {
                            calculate_quantity_component_dim_names(quantity, Named("component.index") = c+1);
                        }
                    }
                    
                    //-- Bind the depends-on quantities --//
                    CharacterVector comp_depends_on = comp["depends.on"];
                    int n_comp_depends_on = comp_depends_on.length();
                    for (int d=0; d<n_comp_depends_on; d++)
                    {
            // Rcout << "  d = " << d << "\n";
                        //std::string dep_on = as<std::string>(depends_on[d]);
                        const char *dep_on = comp_depends_on[d];
                        List after_values_for_comp = i_quantity_after_values[dep_on];

                        bool dep_on_has_after = get_list_elem_by_name(after_values_for_comp, char_time) != R_NilValue;
                        
             //          Rcout << "    is_after_time = " << is_after_time << ", dep_on_has_after = " << dep_on_has_after << "\n";
             
             
                        if ((!is_after_time || dep_on_has_after) &&
                            (t==0 || !i_quantity_is_static[dep_on]))
                        {
                            List all_values;
                            NumericVector values;
                            
                            if (i_quantity_is_static[dep_on])
                            {
                                all_values = i_quantity_values[dep_on];
                                values = all_values["all"];
                            }
                            else if (is_after_time)
                            {
                                all_values = i_quantity_after_values[dep_on];
                                values = all_values[char_time];
                            }
                            else
                            {
                                all_values = i_quantity_values[dep_on];
                                values = all_values[char_time];
                            }
                            
                            if (!strcmp(comp["value.type"], "expression"))
                            {
                                List i_quantity_mapping_indices = engine["i.quantity.mapping.indices"];
                        
                                RObject i_quantity_mapping_indices_for_quantity = get_list_elem_by_name(i_quantity_mapping_indices, quantity_name);
                                bool need_to_calculate_depends_on_indices = i_quantity_mapping_indices_for_quantity == R_NilValue;

                                if (!need_to_calculate_depends_on_indices)                                                    
                                {
                                    List quantity_components_depends_on_indices = ((List) i_quantity_mapping_indices_for_quantity)["components.depends.on"];
                                    
                                    need_to_calculate_depends_on_indices = quantity_components_depends_on_indices.length() <= c;
                                    if (!need_to_calculate_depends_on_indices)
                                    {
                                        List quantity_components_depends_on_indices_c = quantity_components_depends_on_indices[c];
                                        need_to_calculate_depends_on_indices = get_list_elem_by_name(quantity_components_depends_on_indices_c, dep_on) == R_NilValue;
                                    }
                                }
                                
                                if (need_to_calculate_depends_on_indices)
                                {
                                    calculate_quantity_component_depends_on_indices(quantity, 
                                                                                    Named("component.index") = c+1,
                                                                                    _["depends.on"] = dep_on);
                                }
                                
                                i_quantity_mapping_indices = engine["i.quantity.mapping.indices"];
                                i_quantity_mapping_indices_for_quantity = i_quantity_mapping_indices[quantity_name];
                                List quantity_components_depends_on_indices = ((List) i_quantity_mapping_indices_for_quantity)["components.depends.on"];
                                List quantity_components_depends_on_indices_c = quantity_components_depends_on_indices[c];
                                
                                NumericVector unindexed_values = values;
                                IntegerVector dep_on_indices = quantity_components_depends_on_indices_c[dep_on];
                                values = NumericVector(dep_on_indices.length());
                              
                                for (int i=0; i<values.length(); i++)
                                    values[i] = unindexed_values[ dep_on_indices[i]-1 ];
                            }
                            
                            bindings[dep_on] = values;
                        }
                        
                // Rcout << "  done with d=" << d << "\n";
                    }
                    bindings_for_component[c] = bindings;
                        
                    //-- Calculate the value --//
                    // Rcout << "Evaluating the value of the component...";

                    Function evaluate_function = comp["evaluate"];
                    NumericVector value = evaluate_function(Named("bindings") = bindings, 
                                                            _["error.prefix"] = error_prefix);
                    // Rcout << "done\n";
                    
                    //-- If a function value.type, check the returned value and set its dim.names if needed --//
                    if (!strcmp(comp["value.type"], "function")) //ie if this IS a function
                    {
                        if (check_consistency)
                        {
                            check_function_quantity_component_value(value,
                                                                    Named("quantity") = quantity,
                                                                    _["component.index"] = c+1,
                                                                    _["time"] = char_time,
                                                                    _["error.prefix"] = error_prefix);
                        }

                        
                        List i_quantity_component_dim_names = engine["i.quantity.component.dim.names"];
                        RObject quantity_component_dim_names = get_list_elem_by_name(i_quantity_component_dim_names, quantity_name);
                        if (quantity_component_dim_names == R_NilValue ||
                            ((List) quantity_component_dim_names).length() <= c ||
                            ((List) quantity_component_dim_names)[c] == R_NilValue || 
                            (check_consistency && t==0))
                        {
                            calculate_quantity_component_dim_names(quantity, 
                                                                   Named("component.index") = c+1,
                                                                   _["value.for.function"] = value);
                        }

                        i_quantity_component_dim_names = engine["i.quantity.component.dim.names"]; //re-pull to get the updated dimnames
                        quantity_component_dim_names = i_quantity_component_dim_names[quantity_name];

                        List dn1 = ((List) quantity_component_dim_names)[c];
                        List dn2 = value.attr("dimnames");
                        bool dn_equal = dim_names_are_equal(dn1, dn2);
                        
                        if (check_consistency && !dn_equal)
                        {
                            CharacterVector msg(3);
                            msg[0] = "The dimnames for the value calculated at time ";
                            msg[1] = char_time;
                            msg[2] = " do not match the dimnames of values for previous times";
                            return (msg);
                        }
                    }
                    //-- Some check --//
                    if (value.length()==0)
                    {
                        CharacterVector msg(3);
                        msg[0] = "The value calculated at time ";
                        msg[1] = char_time;
                        msg[2] = " had length zero";
                        return (msg);
                    }
                    
                    // for (int i=0; i<value.length(); i++)
                    // {
                    //     if (value[i]==NA_REAL)
                    //     {
                    //         CharacterVector msg(3);
                    //         msg[0] = "The value calculated at time ";
                    //         msg[1] = time;
                    //         msg[2] = " contained one or more NA values";
                    //         return (msg);
                    //     }
                    // }
                    
                    //-- Package up the value --//
                    component_values[c] = value;
                    // Rcout << "Done calculating the value of component " << c << "\n";
                }
                
                //-- Recalculate the dim.names if needed --//
                
                // Rcout << "Recalculate the dim.names...\n";
                List i_quantity_dim_names = engine["i.quantity.dim.names"];
                if (get_list_elem_by_name(i_quantity_dim_names, quantity_name) == R_NilValue)
                {
                    calculate_quantity_dim_names(quantity);    
                }
                
                //-- Incorporate each component into the quantity value --//
                NumericVector quant_value;
                // Rcout << "Incorporate into quant_value...\n";
                
                for (int c=0; c<n_components; c++)
                {
                    List comp = components[c];
                    NumericVector comp_value = component_values[c];
                    
                    //-- Recalculate the indices if we need to --//
        // Rcout << "Recalculating indices...\n";
                    List i_quantity_mapping_indices = engine["i.quantity.mapping.indices"];
                    bool need_to_calculate_indices = i_quantity_mapping_indices.length() == 0;
                    
                    if (!need_to_calculate_indices)
                    {
                        RObject quantity_mapping_indices = get_list_elem_by_name(i_quantity_mapping_indices, quantity_name);
                        need_to_calculate_indices = quantity_mapping_indices == R_NilValue;
                        
                        if (!need_to_calculate_indices)
                        {
                            RObject components_expand_indices = get_list_elem_by_name((List) quantity_mapping_indices, "components.expand");
                            need_to_calculate_indices = components_expand_indices == R_NilValue ||
                                ((List) components_expand_indices).length() <= c ||
                                ((List) components_expand_indices)[c] == R_NilValue;
                        }
                    }
                    
                    if (need_to_calculate_indices)
                    {
                        calculate_quantity_component_expand_access_indices(quantity,
                                                                           Named("component.index") = c+1);
                    }
                    
                    //-- Pull the expand indices --//
        // Rcout << "Pulling expand indices...\n";
                    i_quantity_mapping_indices = engine["i.quantity.mapping.indices"];
        //Rcout << "i_quantity_mapping_indices_2.length() = " << i_quantity_mapping_indices_2.length() << "\n";
                    List quantity_mapping_indices = i_quantity_mapping_indices[quantity_name];
                    List components_expand_indices = quantity_mapping_indices["components.expand"];
                    IntegerVector expand_indices = components_expand_indices[c];
    
                    //-- Fold it in to the comp.value --//
        // Rcout << "Folding in the value...\n";
                    if (c==0) // just expand - it's the first component
                    {
                        quant_value = NumericVector(expand_indices.length());
                        for (int i=0; i<expand_indices.length(); i++)
                            quant_value[i] = comp_value[ expand_indices[i]-1 ];
                    }
                    else
                    {
                        List components_access_indices = quantity_mapping_indices["components.access"];
                        IntegerVector access_indices = components_access_indices[c];
                        
                     //   std::string apply_function = as<std::string>(comp["apply.function"]);
                        const char *apply_function = comp["apply.function"];
                        if (!strcmp(apply_function, "overwrite"))
                        {                       
                            for (int i=0; i<expand_indices.length(); i++)
                                quant_value[ access_indices[i]-1 ] = comp_value[ expand_indices[i]-1 ];
                        }
                        else if (!strcmp(apply_function, "add"))
                        {                       
                            for (int i=0; i<expand_indices.length(); i++)
                                quant_value[ access_indices[i]-1 ] += comp_value[ expand_indices[i]-1 ];
                        }
                        else if (!strcmp(apply_function, "subtract"))
                        {                       
                            for (int i=0; i<expand_indices.length(); i++)
                                quant_value[ access_indices[i]-1 ] -= comp_value[ expand_indices[i]-1 ];
                        }
                        else if (!strcmp(apply_function, "multiply"))
                        {                       
                            for (int i=0; i<expand_indices.length(); i++)
                                quant_value[ access_indices[i]-1 ] *= comp_value[ expand_indices[i]-1 ];
                        }
                        else if (!strcmp(apply_function, "divide"))
                        {                       
                            for (int i=0; i<expand_indices.length(); i++)
                                quant_value[ access_indices[i]-1 ] /= comp_value[ expand_indices[i]-1 ];
                        }
                        else
                        {
                            CharacterVector msg(5);
                            msg[0] = "Invalid apply.function '";
                            msg[1] = apply_function;
                            msg[2] = "' for model quantity '";
                            msg[3] = quantity_name;
                            msg[4] = "'. Must be one of 'overwrite', 'add', 'subtract', 'multiply', or 'divide'";
                            
                            return (msg); 
                        }
                    }
                }
                
                //-- Some Checks --//
                
                // Rcout << "Performing checks...\n";
                if (quant_value.length()==0)
                {
                    CharacterVector msg(3);
                    msg[0] = "The quant_value calculated at time ";
                    msg[1] = char_time;
                    msg[2] = " had length zero";
                    return (msg);
                }
                
                for (int i=0; i<quant_value.length(); i++)
                {
                    if (quant_value[i]==NA_REAL)
                    {
                        CharacterVector msg(3);
                        msg[0] = "The quant_value calculated at time ";
                        msg[1] = char_time;
                        msg[2] = " contained one or more NA values";
                        return (msg);
                    }
                }
                
                //-- Set the dimnames --//
                // Rcout << "Set the dimnames...\n";
                i_quantity_dim_names = engine["i.quantity.dim.names"];
        // Rcout << "get quantity dim names...";
                List quantity_dim_names = i_quantity_dim_names[quantity_name];
        // Rcout << "SUCCESS\n";
                if (quantity_dim_names.length() > 0)
                {
                    IntegerVector dim(quantity_dim_names.length());
                    for (int i=0; i<dim.length(); i++)
                        dim[i] = ((CharacterVector) quantity_dim_names[i]).length();
                        
                    dim.attr("names") = quantity_dim_names.attr("names");
                    quant_value.attr("dim") = dim;
                    quant_value.attr("dimnames") = quantity_dim_names;
                }
                
                quant_value_object = quant_value;
            }
            
            //-- Store the value --//
// Rcout << "Store the value...\n";
            if (is_after_time)
                quantity_after_values[t] = quant_value_object;
            else
                quantity_values[t] = quant_value_object;
// Rcout << "Done with the loop on iter\n";
        }
    }
    
    //-- Package up and return --//
    return (List::create(Named("quantity.values") = quantity_values,
                 _["quantity.after.values"] = quantity_after_values));
}

