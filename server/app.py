from flask import Flask


app = Flask(__name__)


@app.after_request
def add_security_headers(response):
    response.headers['X-Frame-Options'] = 'SAMEORIGIN'
    response.headers['X-Content-Type-Options'] = 'nosniff'
    response.headers['X-XSS-Protection'] = '1; mode=block'
    return response


@app.route('/')
def index():
    return {'message': 'Banking API Server', 'status': 'running'}


@app.route('/health')
def health():
    return {'status': 'healthy'}


if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=False)
