# -*- coding: utf-8 -*-
"""
Created on Tue Jan 19 16:25:11 2021

@author: ySchmidtM
"""
# Das Programm soll den Inhalt des Master-Input-Files auf die entsprechenden Input-Files aufteilen
# Dafür müssen Daten erkannt und kopiert werden

# INPUT-FILE: WPS_MATERIAL_MECH_MECHANISCH
FILENAME = "Job-1.inp"

do_list = ['*Material, name=Al5182','*Elastic','*Plastic'] 
enddo_list = ['*Depvar','*Latent Heat','*User Defined Field']

index_list = ['*Material, name=CuCrZr']
end_list = ['** INTERACTION PROPERTIES']

def main():
    with open('WPS_MECH_MATERIAL_MECHANISCH.txt', 'w') as f: # Erstellung des Input-Files
        f.write('**')
        f.write('\n')
        f.write('**Bei CuCrZr ist nur Elastic relevent, Rest loeschen')
        f.write('\n')
        f.write('**')
        f.write('\n')
        for i in range(len(do_list)) and range(len(enddo_list)):
            with open(FILENAME, encoding="ascii") as input_file: # Daten aus dem Master-Input-File werden eingelesen
                lines = input_file.readlines() # Dokument wird in einzelne Zeilen unterteilt
                for line in lines:
                    if line.startswith(do_list[i]): # Schlagwortsuche
                        start_line = lines.index(line)
                    elif line.startswith(enddo_list[i]): # Eingrenzung des Bereichs
                        end_line = lines.index(line)
                Resulting_list = lines[start_line:end_line] # Bereich wird genauer definiert                
                f.write(''.join(Resulting_list)) # Einfügen des kopierten Bereichs                
        #f.write('*ANNEAL TEMPERATURE')
        #f.write('\n')
        #f.write('600')
        #f.write('\n')
        
        for line in lines:
            if line.startswith(index_list[0]): # Schlagwortsuche
                start_line1 = lines.index(line)
            elif line.startswith(end_list[0]): # Eingrenzung des Bereichs
                end_line1 = lines.index(line)
                    
        Resulting_list1 = lines[start_line1:end_line1-7]
        f.write('**')
        f.write('\n')
        f.write(''.join(Resulting_list1))
        f.write('**')

if __name__ == "__main__":
    main()



# INPUT-FILE: WPS_Elek_Contact
# FILENAME = "Job-1.inp" # Das Programm und die Textdatei im gleichen Verzeichnis
ip_list = ['*Gap Conductance','*Surface Interaction, name=ELEKTRODE']
end1_list = ['*Surface Interaction, name=BLECH_NOSEP','*Surface Interaction, name=ELEK_NOSEP']

i_list = ['** Interaction: ELEKTRODE_1','** Interaction: BLECH_1','** Interaction: ELEKTRODE_2']
end_list = ['SURF_BL1_EL1','SURF_BL2_BL1','SURF_BL2_EL2']

def main():
    with open('WPS_Elek_Contact.txt', 'w') as file:
        file.write('**')
        file.write('\n')
        file.write('** INTERACTION PROPERTIES')
        file.write('\n')
        file.write('**')
        file.write('\n')
        file.write('*Surface Interaction, name=BLECH')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('*Gap Conductance')
        file.write('\n')
        with open(FILENAME, encoding="ascii") as input_file:
            lines = input_file.readlines()
            for line in lines:
                if line.startswith(ip_list[0]):
                    start_line = lines.index(line)
                elif line.startswith(end1_list[0]):
                    end_line = lines.index(line)
            Resulting_list1 = lines[start_line+1:end_line]
            file.write(''.join(Resulting_list1))
        file.write('**')
        file.write('\n')
        file.write('*GAP ELECTRICAL CONDUCTANCE, user')
        file.write('\n')
        file.write('*Surface Behavior, pressure-overclosure=HARD, no separation')
        file.write('\n')
        file.write('**')
        file.write('\n')
        file.write('**')
        file.write('\n')
        file.write('**')
        file.write('\n')
        file.write('*Surface Interaction, name=ELEKTRODE')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('*Gap Conductance')
        file.write('\n')
        with open(FILENAME, encoding="ascii") as input_file:
            lines = input_file.readlines()
            for line in lines:
                if line.startswith(ip_list[1]):
                    start_line = lines.index(line)
                elif line.startswith(end1_list[1]):
                    end_line = lines.index(line)
            Resulting_list2 = lines[start_line+6:end_line]
            file.write(''.join(Resulting_list2))
        file.write('**')
        file.write('\n')
        file.write('*GAP ELECTRICAL CONDUCTANCE, user')
        file.write('\n')
        file.write('*Surface Behavior, pressure-overclosure=HARD, no separation')
        file.write('\n')
        file.write('**')
        file.write('\n')
        file.write('**')
        file.write('\n')
        file.write('** INTERACTIONS')
        file.write('\n')
        file.write('**\n')
        for i in range(len(i_list)) and range(len(end_list)):
            with open(FILENAME, encoding="ascii") as input_file:
                lines = input_file.readlines()
                for line in lines:
                    if line.startswith(i_list[i]):
                        start_line = lines.index(line)
                    elif line.startswith(end_list[i]):
                        end_line = lines.index(line)
                Resulting_list = lines[start_line:end_line+1]
                file.write(''.join(Resulting_list))
        file.write('**')
if __name__ == "__main__":
    main()
    
    

# WPS_MECH_Netz_Assembly
# FILENAME = "Job-1.inp" # Das Programm und die Textdatei im gleichen Verzeichnis
index_list = ['*Heading','** PART INSTANCE: Blech-1','** PART INSTANCE: Elektrode_oben',
              '** PART INSTANCE: Blech-2','** PART INSTANCE: Elektrode_unten',
              '*Elset, elset=EALL','*Nset, nset=NALL,','*Elset, elset=SET-1',
              '*Elset, elset=SET-2','*Elset, elset=SET-3',
              '*Equation','*Nset, nset=BC_ELEKTRODE','*Nset, nset=NSET_TEMP',
              '*Nset, nset=NSET_E1','*Nset, nset=NSET_E2']

end_list = ['** ---','*Nset, nset=Blech-1','*Nset, nset=Elektrode_oben',
            '*Nset, nset=Blech-2','*Nset, nset=Elektrode_unten',
            '*Nset, nset=ELEKTRODE1','*Elset, elset=NALL,','*Nset, nset=SET-2',
            '*Nset, nset=SET-3','** Constraint: Eqn-1',
            '** MATERIALS','*Elset, elset=BC_ELEKTRODE','*Elset, elset=NSET_TEMP',
            '*Elset, elset=NSET_E1','*Elset, elset=NSET_E2']

header_list = ['nset=LOAD_ELEKTRODE','nset=BC_TEMP','BC_Blechfix']
temp_list = []
all_items = []
header = ''

def main():
    with open('WPS_MECH_Netz_Assembly.txt', 'w') as file:
        for i in range(len(index_list)) and range(len(end_list)):
            with open(FILENAME, encoding="ascii") as input_file:
                lines = input_file.readlines()
                for line in lines:
                    if line.startswith(index_list[i]):
                        start_line = lines.index(line)
                    elif line.startswith(end_list[i]):
                        end_line = lines.index(line)
                Resulting_list = lines[start_line:end_line]
                file.write(''.join(Resulting_list))
                file.write('**')
                file.write('\n')
                file.write('**')
                file.write('\n')
        file.write('*SOLID SECTION, ELSET=SET-1, MATERIAL=CuCrZr')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('*SOLID SECTION, ELSET=SET-2, MATERIAL=Al5182')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('*SOLID SECTION, ELSET=SET-3, MATERIAL=Al5182')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('**')
        file.write('\n')
# drei Begriff werden gesucht: nset=BC_TEMP, BC_Blechfix und
# nset=LOAD_ELEKTRODE (beinhaltet auch nset=LOAD_ELEKTRODE_REFERENZ)
        with open(FILENAME, encoding="ascii") as input_file:            
            lines = input_file.readlines()
            for line in lines: #read each line
                if line.startswith('*') and not(line.startswith('**')) : #find header
                    if temp_list: #check if list is filled with items
                        appendlist = [header]
                        appendlist.append(list(temp_list))
                        
                        all_items.append(appendlist)
                        temp_list.clear()
                        header = line
                    else:
                        header = line
                else:
                    temp_list.append(line)

        def search(header_list):
            for element in all_items:
                if header_list in element[0]: #element[0] == header
                    #print(element[0])
                    file.write(element[0])
                    for x in element[1]:
                        #print(x)
                        file.write(x)
                        file.write('**')
                        file.write('\n')
        
        search(header_list[0])
        search(header_list[1])
        search(header_list[2])

if __name__ == "__main__":
    main()
    
# überflüssige Zeilen bzw. Bestandteile: Surf_load_Current



# INPUT-FILE: WPS_Elek_Elemente
# FILENAME = "Job-1.inp" # Das Programm und die Textdatei im gleichen Verzeichnis

header_list = ['Element, type','Elset, elset=EALL','Elset, elset=SET',
               'Nset, nset=BC_ELEKTRODE','Nset, nset=LOAD_ELEKTRODE',
               'Nset, nset=LOAD_CURRENT','Nset, nset=BC_TEMP','Nset, nset=NSET_E1']
temp_list = []
all_items = []
header = ''

do_list = ['*Material, name=Al5182','*Electrical Conductivity','*Latent Heat','*Specific Heat'] 
enddo_list = ['*Depvar','*Expansion','*Plastic','*User Defined Field']

index_list = ['*Material, name=CuCrZr']
end_list = ['** INTERACTION PROPERTIES']
 

import re

def map_element_header_to_DCAX(header_line: str) -> str:
    s = header_line.rstrip('\n')
    def repl(m):
        t = m.group(1).upper()
        mapping = {
            'CAX3':  'DCAX3E',
            'CAX3R': 'DCAX3E',
            'CAX4':  'DCAX4E',
            'CAX4R': 'DCAX4E',
        }
        if t in mapping:
            return 'TYPE=' + mapping[t]
        # 已经是 DCAX*E 的就不动
        if t.startswith('DCAX') and t.endswith('E'):
            return 'TYPE=' + m.group(1)
        raise SystemExit(f"FATAL: Unsupported element TYPE={m.group(1)} in ELEK elements")

    # 正则：匹配 "*Element, ... type=XXXX" 中的 XXXX
    s2 = re.sub(r'(?i)TYPE\s*=\s*([A-Z0-9]+)', repl, s, count=1)
    return s2 + '\n'



def main():
    with open('WPS_Elek_Elemente.txt', 'w') as file:
        file.write('**')
        file.write('\n')
        file.write('*SOLID SECTION, ELSET=SET-1, MATERIAL=CuCrZr')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('*SOLID SECTION, ELSET=SET-2, MATERIAL=Al5182')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('*SOLID SECTION, ELSET=SET-3, MATERIAL=Al5182')
        file.write('\n')
        file.write('1.,')
        file.write('\n')
        file.write('**')
        file.write('\n')
        with open(FILENAME, encoding="ascii") as input_file:            
            lines = input_file.readlines()
            for line in lines: #read each line
                if line.startswith('*') and not(line.startswith('**')) : #finde Überschrift
                    if temp_list: #check if list is filled with items
                        appendlist = [header]
                        appendlist.append(list(temp_list))
                        
                        all_items.append(appendlist)
                        temp_list.clear()
                        header = line
                    else:
                        header = line
                else:
                    temp_list.append(line)

        # def search(header_list):
        #     for element in all_items:
        #         if header_list in element[0]: #element[0] == header
        #             #print(element[0])
        #             file.write(element[0])
        #             for x in element[1]:
        #                 #print(x)
        #                 file.write(x)
        #             file.write('**')
        #             file.write('\n')

        def search(header_list):
            for element in all_items:
                if header_list in element[0]:  # element[0] 是块头
                    header_line = element[0]
                    if header_list.startswith('Element, type'):
                        # ★ 把 CAX* → DCAX*E
                        header_line = map_element_header_to_DCAX(header_line)
                    file.write(header_line)
                    for x in element[1]:
                        file.write(x)
                    file.write('**\n')
        
        search(header_list[0])
        search(header_list[1])
        search(header_list[2])
        search(header_list[3])
        search(header_list[4])
        search(header_list[5])
        search(header_list[6])
        search(header_list[7])
        with open(FILENAME, encoding="ascii") as input_file:
            lines = input_file.readlines()
            for line in lines:
                if line.startswith('_SURF2_'):
                    start_line = lines.index(line)
                elif line.startswith('** Constraint: Eqn-1'):
                    end_line = lines.index(line)
            Resulting_list = lines[start_line+1:end_line]
            file.write(''.join(Resulting_list))
            file.write('**')
            file.write('\n')

        file.write('**')
        file.write('\n')
        for i in range(len(do_list)) and range(len(enddo_list)):
            with open(FILENAME, encoding="ascii") as input_file: # Daten aus dem Master-Input-File werden eingelesen
                lines = input_file.readlines() # Dokument wird in einzelne Zeilen unterteilt
                for line in lines:
                    if line.startswith(do_list[i]): # Schlagwortsuche
                        start_line = lines.index(line)
                    elif line.startswith(enddo_list[i]): # Eingerenzung des Bereichs
                        end_line = lines.index(line)
                Resulting_list = lines[start_line:end_line] # Bereich wird genauer definiert                
                file.write(''.join(Resulting_list)) # Einfügen des kopierten Bereichs                
                
        for line in lines:
            if line.startswith(index_list[0]): # Schlagwortsuche
                start_line1 = lines.index(line)
            elif line.startswith(end_list[0]): # Eingrenzung des Bereichs
                end_line1 = lines.index(line)
                    
        Resulting_list1 = lines[start_line1:end_line1-3]
        file.write('**')
        file.write('\n')
        file.write(''.join(Resulting_list1))
        file.write('**')

if __name__ == "__main__":
    main()