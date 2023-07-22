package com.atorres.nttdata.prodpasivems.client;

import com.atorres.nttdata.prodpasivems.model.creditms.CreditDao;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Flux;

@Component
public class ClientApiProdActive {
	private final WebClient client;

	public ClientApiProdActive(WebClient.Builder webClientBuilder) {
		String applicationName = "prod-active-ms"; // Reemplaza "nombre-de-aplicacion" con el nombre real de la aplicación
		this.client = webClientBuilder
						.baseUrl("http://" + applicationName + "/api/credit")
						.defaultHeader(HttpHeaders.ACCEPT, MediaType.TEXT_EVENT_STREAM_VALUE)
						.build();
	}

	public Flux<CreditDao> getCredit(){
		return client.get()
						.uri("/")
						.retrieve()
						.bodyToFlux(CreditDao.class);
	}

	public Flux<CreditDao> getCreditByClient(String idclient){
		return client.get()
						.uri("/{idclient}", idclient)
						.retrieve()
						.bodyToFlux(CreditDao.class);
	}
}
