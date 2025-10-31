import requests
from ids import ids
import json
import pickle

lang = 'zh'
api_url = "https://onepiece.fandom.com/{}/api/v1".format(lang)
raw_data_dir = 'raw_data/{}'.format(lang)
data_dir = 'data/{}'.format(lang)
serialized_dir = 'serialized/{}'.format(lang)

# Stage 1: Get page ids
# r = requests.get("{}/Articles/List?category=角色&limit=1000".format(api_url))
# for i in r.json()['items']:
#     print(i['id'])

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
#             par = section['level']
#             for item in section['content']:
#                 if item['type'] == 'paragraph':
#                     entity['texts'] += list(map(lambda x: (x).replace('\xa0', ' ') + '。', filter(lambda x: x != '', item['text'].split('。'))))
#         if not (entity['name'].endswith('角色') or '的' in entity['name']):
#             res.append(entity)
# with open('{}/res.pic'.format(serialized_dir), 'wb') as outfile:
#     pickle.dump(res, outfile)

# Stage 4: Save as texts.
# with open('{}/res.pic'.format(serialized_dir), 'rb') as infile:
#     res = pickle.load(infile)
#     for entity in res:
#         with open('{}/{}.txt'.format(data_dir, entity['name']), 'w') as outfile:
#             outfile.writelines(map(lambda x: x + '\n', entity['texts']))
