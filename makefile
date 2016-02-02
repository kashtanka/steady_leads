#makefile for single-column version of NH3D
model_path= /home/dmitry/models/steady_leads/
source_path = /home/dmitry/models/steady_leads/source
obj_path = /home/dmitry/models/steady_leads/obj
exec = sl.out
fc = ifort
switch = -r8 -O3 -traceback
obj = \
 $(obj_path)/steady_mod.o \
 $(obj_path)/main.o \
 $(obj_path)/balance.o \
 $(obj_path)/surf_layer_t.o \
#
$(exec) : $(obj)
	$(fc) -o $(exec) $(switch) $(obj)
#MODULES:
$(obj_path)/alloc_1d_mod.o : $(source_path)/steady_mod.f	
	cd $(obj_path)/ && $(fc) -c $(switch) $(source_path)/alloc_1d_mod.for && cd $(model_path)
#OTHER SOURCE
$(obj_path)/%.o : $(source_path)/%.for
	cd $(obj_path)/ && $(fc) -c $(switch) $< && cd $(model_path)
$(obj_path)/%.o : $(source_path)/%.f
	cd $(obj_path)/ && $(fc) -c $(switch) $< && cd $(model_path)
clean :
	rm $(obj) $(exec)