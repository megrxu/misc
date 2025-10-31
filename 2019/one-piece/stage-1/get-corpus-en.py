import requests, json, pickle, nltk.data
from ids import ids_en as ids
from stanfordcorenlp import *

tokenizer = nltk.data.load('tokenizers/punkt/english.pickle')

lang = 'en'
api_url = "https://onepiece.fandom.com/api/v1"
raw_data_dir = 'raw_data/{}'.format(lang)
data_dir = 'data/{}'.format(lang)
serialized_dir = 'serialized/{}'.format(lang)

# # Stage 1: Get page ids
# url = "https://onepiece.fandom.com/api/v1/Articles/List?category={}&limit=1000"
# ids = set()
# queue = set(['Characters'])
# isvisited = dict()
# depth = 0
# while len(queue) >= 1:
#     flag = False
#     print('len=', len(queue))
#     category = queue.pop()
#     if isvisited.get(category) == True:
#         continue
#     else:
#         isvisited[category] = True
#     r = requests.get(url.format(category))
#     depth += 1
#     try:
#         for i in r.json()['items']:
#             if 'Category:' in i['url']:
#                 if depth < 5:
#                     flag = True
#                     queue.add(i['url'].split(':')[-1])
#             else:
#                 ids.add(i['id'])
#     except:
#         pass
#     if not flag:
#         depth -= 1

# Stage 2: Get pages
# for _id in ids:
#     with open('{}/{}.json'.format(raw_data_dir, _id), 'w') as outfile:
#         r = requests.get("{}/Articles/AsSimpleJson?id={}".format(api_url, _id))
#         json.dump(r.json(), outfile, ensure_ascii=False)

# Stage 3: Extract entity names and sentances from pages (as a dict)
# res = list()
# for _id in ids:
#     with open('{}/{}.json'.format(raw_data_dir, _id), 'r') as infile:
#         entity = dict()
#         page = json.load(infile)
#         entity['id'] = _id
#         entity['texts'] = list()
#         for section in page['sections']:
#             if section['level'] == 1:
#                 entity['name'] = section['title']
#             else:
#                 break
#             for item in section['content']:
#                 if item['type'] == 'paragraph':
#                     entity['texts'] += list(tokenizer.tokenize(item['text'].replace(u'\xa0', u' ')))
#         res.append(entity)
# with open('{}/res-lv1.pic'.format(serialized_dir), 'wb') as outfile:
#     pickle.dump(res, outfile)

# Stage 4: Save as texts.
with open('{}/res-lv1.pic'.format(serialized_dir), 'rb') as infile:
    res = pickle.load(infile)
    for entity in res:
        with open('{}/{}.txt'.format(data_dir, entity['name'].replace('/', '-')), 'w') as outfile:
            outfile.writelines(map(lambda x: x + '\n', entity['texts']))
