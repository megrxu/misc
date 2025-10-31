
# name_list = []
# with open('./filelist') as file:
#     for line in file.readlines():
#         name_list += line.replace('.', '')[0:-4].split(' ')

# with open('triples.ollie') as file:
#     namefile = open('triples-withnames.ollie', 'w')
#     for line in file.readlines():
#         for name in name_list:
#             if name in line.split(';')[-1]:
#                 namefile.writelines([line])
#                 break

relations = set()
with open('triples.ollie') as file:
    for line in file.readlines():
        rel = line.split(';')[-2]
        if 'son of' in rel :
        # if 'is' in rel and 'of' in rel or ('has' in rel):
            relations.add(rel)
for i in relations:
    print(i)