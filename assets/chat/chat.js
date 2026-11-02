(function(){'use strict';
// Xeon's chat body: request cards and the composer. The transcript comes from the
// backend's chat page; provider settings, usage and approvals live in native
// controls above this view. Messages go to Xeon through LGT's local bridge.
const post=data=>{if(window.lgt)window.lgt.postMessage(JSON.stringify(data))};
window.addEventListener('xeon-message',event=>post(event.detail));
const node=(tag,cls,text)=>{const e=document.createElement(tag);if(cls)e.className=cls;if(text!==undefined)e.textContent=text;return e};
let state=null,promptSignature='',lastErrorRevision=-1;

const composer=node('section');composer.id='composer';composer.setAttribute('aria-label','Chat composer');
const toast=node('div');toast.id='toast';toast.setAttribute('role','status');
const requests=node('div');requests.id='requests';requests.setAttribute('aria-label','Assistant requests');
const inner=node('div');inner.id='composer-inner';
const attached=node('div');attached.id='attachments';
const input=node('textarea');input.id='chat-input';input.setAttribute('aria-label','Message the assistant');
input.placeholder='Ask about your project, or describe what you want to build…';
const actions=node('div','compose-actions'),hint=node('span','hint','Return to send · Shift+Return for a new line · Paste or drop screenshots'),buttons=node('div','buttons');
const stop=node('button','secondary','Stop');stop.id='stop';stop.addEventListener('click',()=>post({type:'interrupt'}));
const submit=node('button','primary','Send');submit.id='send';submit.addEventListener('click',()=>window.xeonInput.submit());
buttons.append(stop,submit);actions.append(hint,buttons);inner.append(attached,input,actions);
composer.append(toast,requests,inner);document.body.append(composer);
new ResizeObserver(()=>document.documentElement.style.setProperty('--composer',composer.offsetHeight+'px')).observe(composer);

function error(message){toast.textContent=message||'';toast.style.display=message?'block':'none'}

function field(schema,label){const wrap=node('label','',label),type=schema.type||'string';let control;
 if(schema.enum){control=node('div');const name='enum-'+Math.random();for(const value of schema.enum){const l=node('label','choice'),r=node('input');r.type='radio';r.name=name;r.value=JSON.stringify(value);l.append(r,document.createTextNode(' '+String(value)));control.append(l)}wrap.append(control);
  return {wrap,value:()=>{const selected=control.querySelector(':checked');return selected?JSON.parse(selected.value):undefined}}}
 control=node('input');control.type=type==='boolean'?'checkbox':type==='integer'||type==='number'?'number':schema.writeOnly||schema.format==='password'?'password':'text';
 if(control.type==='number')control.step=type==='integer'?'1':'any';wrap.append(control);
 return {wrap,value:()=>{if(type==='boolean')return control.checked;if(type==='number'||type==='integer'){if(control.value==='')return undefined;const n=Number(control.value);if(!Number.isFinite(n)||(type==='integer'&&!Number.isInteger(n)))throw Error('Enter a valid '+type);return n}return control.value}}}

const decisionLabel=d=>{const key=typeof d==='string'?d:Object.keys(d||{})[0];
 return ({accept:'Allow once',decline:'Decline',cancel:'Cancel',acceptForSession:'Allow for this session',acceptWithExecpolicyAmendment:'Allow and remember command',applyNetworkPolicyAmendment:'Apply network policy'}[key]||'Review and allow')};

function renderRequests(prompts){const signature=JSON.stringify(prompts);if(signature===promptSignature)return;promptSignature=signature;requests.replaceChildren();
 for(const p of prompts){const card=node('section','request-card kind-'+p.kind);
  const head=node('div','request-head');head.append(node('span','request-badge',({approval:'Approval',questions:'Question',elicitation:'Request',json:'Request'}[p.kind]||'Request')),node('h3','request-title',p.title||'Assistant request'));card.append(head);
  if(p.summary)card.append(node('pre','request-summary',p.summary));
  if(p.details){const details=node('details'),summary=node('summary','','Details');details.append(summary,node('pre','',p.details));if(!p.summary)details.open=true;card.append(details)}
  const getters=[];
  if(p.kind==='questions'){for(const [index,q] of p.questions.entries()){const set=node('fieldset'),legend=node('legend','',q.label||q.id);set.append(legend);const options=[];
    for(const option of q.options||[]){const l=node('label','choice'),c=node('input');c.type=q.multiple?'checkbox':'radio';c.name='q-'+p.id+'-'+index;c.value=option.label;l.append(c,document.createTextNode(' '+option.label));if(option.description)l.append(node('small','',option.description));set.append(l);options.push(c)}
    const other=node('input');other.type=q.secret?'password':'text';if(q.secret)other.autocomplete='off';other.placeholder=options.length?'Or write your own answer…':(q.secret?'Secret value':'Your answer');other.setAttribute('aria-label',q.label||q.id);set.append(other);card.append(set);
    getters.push(()=>{const selected=options.filter(x=>x.checked).map(x=>x.value);const values=other.value.trim()?(q.multiple?[...selected,other.value]:[other.value]):selected;if(!values.length)throw Error('Answer: '+(q.label||q.id));return [q.id,values]})}}
  let schemaValue=()=>null;
  if(p.kind==='elicitation'&&!p.confirmation&&p.url){const open=node('button','secondary','Open link');open.addEventListener('click',()=>post({type:'open-link',url:p.url}));card.append(open)}
  else if(p.kind==='json'||(p.kind==='elicitation'&&!p.confirmation)){
   const schema=p.schema||{},supported=['string','number','integer','boolean'];
   if(schema.type==='object'&&schema.properties&&Object.values(schema.properties).every(s=>supported.includes(s.type)||Array.isArray(s.enum))){const fields=[];
    for(const [key,s] of Object.entries(schema.properties)){const f=field(s,s.title||key);card.append(f.wrap);fields.push([key,f,schema.required?.includes(key)])}
    schemaValue=()=>{const result={};for(const [key,f,required] of fields){const v=f.value();if(required&&(v===''||v===undefined))throw Error('Enter '+key);if(v!==undefined&&(required||v!==''))result[key]=v}return result}}
   else{const label=node('label','','Structured response (JSON)'),area=node('textarea');area.setAttribute('aria-label','Structured JSON response');area.value=schema.type==='object'?'{}':'';label.append(area);card.append(label);
    const schemaDetails=node('details');schemaDetails.append(node('summary','','Response schema'),node('pre','',JSON.stringify(schema,null,2)));card.append(schemaDetails);
    schemaValue=()=>{try{return JSON.parse(area.value)}catch{throw Error('Enter valid JSON before submitting.')}}}}
  const localError=node('div','request-error');localError.setAttribute('role','alert');card.append(localError);
  const row=node('div','prompt-actions');
  const offered=p.kind==='approval'&&p.decisions?.length?p.decisions.map(d=>({label:decisionLabel(d),decision:d})):
   p.kind==='questions'?[{label:'Cancel',decision:'cancel'},{label:'Submit answers',decision:'allow'}]:
   [{label:'Decline',decision:'deny'},{label:p.kind==='approval'?'Allow once':'Submit',decision:'allow'}];
  for(const item of offered){const deny=['deny','decline','cancel'].includes(item.decision),b=node('button',deny?'secondary':'primary',item.label);
   b.dataset.chatDenied=String(p.kind==='approval'&&!deny);b.disabled=state?.mode==='chat'&&b.dataset.chatDenied==='true';
   b.addEventListener('click',()=>{try{const answers=deny?null:p.kind==='questions'?Object.fromEntries(getters.map(g=>g())):schemaValue();
    post({type:'answer',id:p.id,generation:p.generation,epoch:p.epoch,decision:item.decision,answers});card.classList.add('pending');for(const button of row.children)button.disabled=true;localError.textContent=''}catch(e){localError.textContent=e.message}});row.append(b)}
  card.append(row);requests.append(card)}
 if(prompts.length)requests.firstElementChild.querySelector('input,textarea,button.primary')?.focus({preventScroll:true})}

window.xeonWorkspace={
 update(data){state=data;if(!data.error){lastErrorRevision=-1;error('')}
  if(data.error&&data.errorRevision!==lastErrorRevision){lastErrorRevision=data.errorRevision;window.xeonInput.ack(false);error(data.error)}
  const busy=['busy','waiting'].includes(data.status),live=['ready','busy','waiting'].includes(data.status);
  submit.disabled=!live;submit.textContent=busy?'Steer':'Send';
  input.placeholder=!live?(data.status==='starting'?'Connecting to the assistant…':'The assistant is not connected. Use Reconnect above.'):busy?'Add guidance to steer the current turn…':'Ask about your project, or describe what you want to build…';
  hint.textContent=busy?'Return to steer · Shift+Return for a new line':'Return to send · Shift+Return for a new line · Paste or drop screenshots';
  stop.style.display=busy?'inline-block':'none';renderRequests(data.requests||[])},
 error,
 attachments(images){attached.replaceChildren();for(const [index,url] of (Array.isArray(images)?images:[]).entries()){const card=node('div','attachment'),image=node('img');image.src=url;image.alt='Attached image '+(index+1);
  const remove=node('button','','×');remove.type='button';remove.setAttribute('aria-label','Remove image '+(index+1));remove.addEventListener('click',()=>{if(!input.readOnly)post({type:'remove-image',index})});card.append(image,remove);attached.append(card)}},
 ack(ok){window.xeonInput.ack(ok);if(ok)error('');for(const c of requests.children){c.classList.remove('pending');for(const b of c.querySelectorAll('.prompt-actions button'))b.disabled=state?.mode==='chat'&&b.dataset.chatDenied==='true'}},
 focus(){input.focus()}};
input.addEventListener('input',()=>error(''));
})();
