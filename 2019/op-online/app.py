from flask import (
    Flask,
    render_template,
    request, Response
)
import quepy
import rdflib

dbquery = quepy.install("dbquery")
g = rdflib.Graph()
g.parse('data/op-data.xml')

# Create the application instance
app = Flask(__name__, template_folder="templates", static_folder="data", static_url_path='/')

# Create a URL route in our application for "/"
@app.route('/')
def index():
    """
    This function just responds to the browser ULR
    localhost:5000/

    :return:        the rendered template 'home.html'
    """
    return render_template('index.html')


@app.route('/ask', methods=['GET', 'POST'])
def ask():
    _, query, _ = dbquery.get_query(request.args.get('q'))

    res = ""
    print query
    if query:
        qres = g.query(query)
        for row in qres:
            res += ("%s\n" % row)
    if res:
        return res
    else:
        return "Sorry, I don't know."


# If we're running in stand alone mode, run the application
if __name__ == '__main__':
    app.run(debug=False)
