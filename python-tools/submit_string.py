import requests
import sys
import encode
import decode


url = 'https://boundvariable.space/communicate'
with open("api-token.txt", "r") as file:
    token = file.read()
headers = {'Authorization': f'Bearer {token}'}
question = sys.argv[1]
encoded = encode.encode(question)
response = requests.post(url=url, headers=headers, data=f"S{encoded}")
decoded = decode.decode(response.content.decode('ascii')[1:])
print(decoded)
